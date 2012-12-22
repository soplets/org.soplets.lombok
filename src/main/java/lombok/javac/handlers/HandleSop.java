package lombok.javac.handlers;

import static lombok.javac.handlers.JavacHandlerUtil.chainDots;
import static lombok.javac.handlers.JavacHandlerUtil.chainDotsString;
import static lombok.javac.handlers.JavacHandlerUtil.deleteAnnotationIfNeccessary;

import java.lang.reflect.Method;
import java.util.Hashtable;
import java.util.StringTokenizer;

import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.javac.JavacAST;
import lombok.javac.JavacAnnotationHandler;
import lombok.javac.JavacNode;
import lombok.javac.JavacResolution.TypeNotConvertibleException;
import lombok.soplets.Sop;

import org.eclipse.core.runtime.CoreException;
import org.mangosdk.spi.ProviderFor;

import com.sun.tools.javac.code.BoundKind;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.comp.Enter;
import com.sun.tools.javac.comp.Env;
import com.sun.tools.javac.jvm.ClassReader;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.JCTree.JCAnnotation;
import com.sun.tools.javac.tree.JCTree.JCBlock;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCIdent;
import com.sun.tools.javac.tree.JCTree.JCMethodDecl;
import com.sun.tools.javac.tree.JCTree.JCMethodInvocation;
import com.sun.tools.javac.tree.JCTree.JCModifiers;
import com.sun.tools.javac.tree.JCTree.JCReturn;
import com.sun.tools.javac.tree.JCTree.JCStatement;
import com.sun.tools.javac.tree.JCTree.JCTypeApply;
import com.sun.tools.javac.tree.JCTree.JCTypeParameter;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;

@ProviderFor(JavacAnnotationHandler.class)
public class HandleSop extends JavacAnnotationHandler<Sop> {

	private final static String SOPLET = "Soplet";
	
	@Override
	public void handle(AnnotationValues<Sop> annotation, JCAnnotation ast, JavacNode annotationNode) {
		deleteAnnotationIfNeccessary(annotationNode, Sop.class);
		
		JavacNode typeNode = annotationNode.up();
		
		if(!isEnum(typeNode)) {
			annotationNode.addError("@Sop is only supported on an enum");
			return;
		}
		if (!(typeNode.get() instanceof JCClassDecl)) {
			return;
		}
				
		try {			
			java.util.List<String> stereoTypeNames = annotation.getProbableFQTypes("aspects");
			if (stereoTypeNames == null || stereoTypeNames.size() == 0) {
				annotationNode.addError("Invalid stereotype");
				return;
			}
			
			if (stereoTypeNames.size() == 0) return;  

			Hashtable<String, MethodDescriptor> attributeNames = new Hashtable<String, MethodDescriptor>();
			JCClassDecl typeDecl = (JCClassDecl) typeNode.get();
			
			for (String stereoTypeName : stereoTypeNames) {
				try {
					//built-in class?
					Class<?> clazz = Class.forName(stereoTypeName);
					addMethodDescriptors(attributeNames, typeNode, clazz);
				} catch (Exception e) {
					//nop, user-class...
					addMethodDescriptors(attributeNames, typeNode.getContext(), stereoTypeName);
				}
				
				//add "implements"
				TreeMaker maker = typeNode.getTreeMaker();
				JCIdent interfaceIdent = maker.Ident(typeNode.toName(stereoTypeName));
				typeDecl.implementing = typeDecl.implementing.append(interfaceIdent);
			}
			
			//make the inner @Soplet class
			createSopletAnnotation(typeDecl, annotationNode, ast, attributeNames);

			for (String attributeName : attributeNames.keySet()) {
				MethodDescriptor descriptor = attributeNames.get(attributeName);	
				createAccessorMethod(typeDecl, annotationNode, descriptor);
			}
			
			makeAnnotationTypeMethod(typeDecl, annotationNode);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/** make method
	 * @Override
	 * public Class<? extends Annotation> annotationType() {
	 *    return Soplet.class;
	 * }  
	 */
	private void makeAnnotationTypeMethod(JCClassDecl typeDecl, JavacNode typeNode) {
		TreeMaker maker = typeNode.getTreeMaker();
		
		JCExpression defaultValue = null;
		JCModifiers mods = maker.Modifiers(Flags.PUBLIC);
		Name name = Names.instance(typeNode.getContext()).fromString("annotationType");
		
		List<JCTypeParameter> typarams = List.nil();
		List<JCVariableDecl> params = List.nil();
		List<JCExpression> thrown = List.nil();

		// return Soplet.class
		JCExpression annotationValue = maker.Ident(typeNode.toName(SOPLET + ".class"));
		JCStatement returnStatement = maker.Return(annotationValue);
		JCBlock block = maker.Block(0, List.of(returnStatement));
		
		// ? extends Annotation
		JCExpression wildcard = maker.Wildcard(maker.TypeBoundKind(BoundKind.EXTENDS), maker.Ident(typeNode.toName("java.lang.annotation.Annotation")));
		
		// Class<? extends Annotation>
		JCTypeApply returnType = maker.TypeApply(chainDotsString(typeNode, "java.lang.Class"), List.of(wildcard));
		
		JCMethodDecl methodDecl = maker.MethodDef(mods, name, returnType, typarams, params, thrown, block, defaultValue);
		JCClassDecl parentEnum = typeDecl;
		parentEnum.defs = parentEnum.defs.append(methodDecl);
		typeNode.add(methodDecl, Kind.METHOD);

	}
	
	private void addMethodDescriptors(Hashtable<String, MethodDescriptor> attributeNames, JavacNode typeNode, Class<?> clazz) {
		for (Method method : clazz.getDeclaredMethods()) {
			//avoid duplicates
			if (attributeNames.containsKey(method.getName())) {
				continue;
			}
			MethodDescriptor descriptor = new MethodDescriptor();
			descriptor.methodName = method.getName();
			attributeNames.put(descriptor.methodName, descriptor);
			
			Class<?> returnTypeClass = method.getReturnType();
			try {
				descriptor.returnType = findJCTypeByFQName(typeNode.getAst(), returnTypeClass.getName());
			} catch (Exception e) {
				e.printStackTrace(); 
			}

			TreeMaker maker = typeNode.getTreeMaker();
			Object defaultValueObject = method.getDefaultValue();
			if (defaultValueObject instanceof Class) {
				String className = ((Class<?>) defaultValueObject).getName();
				if (className.endsWith("$None")) {
					//no default value, just skip it
					return;
				}
				
				try {
					descriptor.defaultValue = maker.Ident(typeNode.getAst().toName(className + ".class"));
				} catch (AssertionError ae) {
					ae.printStackTrace();			
				}
			} else if (defaultValueObject instanceof Enum<?>) {
				String className = ((Enum<?>)defaultValueObject).getClass().getCanonicalName();
				String enumName = ((Enum<?>)defaultValueObject).name();
				try {
					descriptor.defaultValue = maker.Ident(typeNode.getAst().toName(className + "." + enumName));
				} catch (AssertionError ae) {
					System.out.println("Error at method: " + method.getName() + " default value : " + defaultValueObject);
					ae.printStackTrace();			
				}
			} else if (defaultValueObject != null){
				try {
					descriptor.defaultValue = maker.Literal(defaultValueObject);
				} catch (AssertionError ae) {
					System.out.println("Error at method: " + method.getName() + " default value : " + defaultValueObject);
					ae.printStackTrace();			
				}
			}
		}		
	}

	private JCExpression findJCTypeByFQName(JavacAST ast, String stereoTypeName) throws CoreException, TypeNotConvertibleException {
		TreeMaker maker = ast.getTreeMaker();
		StringTokenizer st = new StringTokenizer(stereoTypeName, ".");
		JCExpression out = maker.Ident(ast.toName(st.nextToken()));
		while (st.hasMoreTokens()) {
			out = maker.Select(out, ast.toName(st.nextToken()));
		}
		return out;
	}
	
	private void addMethodDescriptors(Hashtable<String, MethodDescriptor> attributeNames, Context context, String stereoTypeName) {
		ClassReader reader = ClassReader.instance(context);
		Name flatname = Names.instance(context).fromString(stereoTypeName);
		ClassSymbol symbol = reader.loadClass(flatname);
		Enter enter = Enter.instance(context);
		Env<?> o3 = enter.getEnv(symbol);
		if (o3 == null) return;
		
		JCClassDecl classDecl = o3.enclClass;
		for (JCTree def : classDecl.defs ) {
			if (!(def instanceof JCMethodDecl)) continue;
			JCMethodDecl mDef = (JCMethodDecl)def;
			MethodDescriptor descriptor = new MethodDescriptor();
			descriptor.methodName = mDef.name.toString();
			if (attributeNames.containsKey(descriptor.methodName)) {
				continue;
			}
			descriptor.defaultValue = mDef.defaultValue;
			descriptor.returnType = mDef.restype;
			attributeNames.put(descriptor.methodName, descriptor);
		}		
	}
	
	private boolean isEnum(JavacNode typeNode) {
		JCClassDecl typeDecl = null;
		if (typeNode.get() instanceof JCClassDecl) typeDecl = (JCClassDecl) typeNode.get();
		if (typeDecl == null) return false;
		//JCModifiers modifiers = typeDecl.mods;		
		//(modifiers &	(ClassFileConstants.AccInterface | ClassFileConstants.AccAnnotation | ClassFileConstants.AccDefault)) != 0;
		return true;
	}

	private JavacNode injectInterfaceDeclaration(JavacNode type, JCClassDecl interfaceDeclaration) {
		JCClassDecl parentEnum = (JCClassDecl) type.up().get();
		parentEnum.defs = parentEnum.defs.append(interfaceDeclaration);
		return type.add(interfaceDeclaration, Kind.TYPE);
	}
	
	private void createSopletAnnotation(JCClassDecl typeDecl, JavacNode typeNode, JCAnnotation sourceAnnotation, Hashtable<String, MethodDescriptor> attributeNames) {
		TreeMaker maker = typeNode.getTreeMaker();
		
		JCTree[] attributes = new JCTree[attributeNames.size()];
		int i = 0;
		for (String attributeName : attributeNames.keySet()) {
			MethodDescriptor descriptor = attributeNames.get(attributeName);				
			JCModifiers mods = maker.Modifiers(Flags.PUBLIC);
			Name name = Names.instance(typeNode.getContext()).fromString(descriptor.methodName);
			List<JCTypeParameter> typarams = List.nil();
			List<JCVariableDecl> params = List.nil();
			List<JCExpression> thrown = List.nil();
			JCBlock block = null;
			JCMethodDecl methodDecl = maker.MethodDef(mods, name, descriptor.returnType, typarams, params, thrown, block, descriptor.defaultValue);
			attributes[i++] = methodDecl;
		}
		
		List<JCTree> defs = List.from(attributes);
		List<JCExpression> implementers = List.nil();		
		Name name = Names.instance(typeNode.getContext()).fromString(SOPLET);
		JCModifiers mods2 = maker.Modifiers(Flags.PRIVATE | Flags.INTERFACE | Flags.ANNOTATION);
		List<JCTypeParameter> typarams2 = List.nil();
		JCClassDecl annotationJCClassDecl = maker.ClassDef(mods2, name, typarams2, null, implementers, defs);
		
		
		//add @java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.RUNTIME)
		JCExpression suppressWarningsType = chainDots(typeNode, "java", "lang", "annotation", "Retention");
		JCIdent runtimeValue = maker.Ident(typeNode.toName("java.lang.annotation.RetentionPolicy.RUNTIME"));
		JCAnnotation annotation = maker.Annotation(suppressWarningsType, List.<JCExpression>of(runtimeValue));
		annotationJCClassDecl.mods.annotations = annotationJCClassDecl.mods.annotations.append(annotation);

		injectInterfaceDeclaration(typeNode, annotationJCClassDecl);
		
	}

	/**
	 * create getter method
	 * 	e.g. return getClass().getField(name()).getAnnotation(Soplet.class).textEN();
	 */
	private void createAccessorMethod(JCClassDecl typeDecl, JavacNode typeNode, MethodDescriptor methodDescriptor) throws TypeNotConvertibleException  {
		
		TreeMaker maker = typeNode.getTreeMaker();
		
		JCExpression defaultValue = null;
		JCModifiers mods = maker.Modifiers(Flags.PUBLIC);
		Name name = Names.instance(typeNode.getContext()).fromString(methodDescriptor.methodName);
		List<JCTypeParameter> typarams = List.nil();
		List<JCVariableDecl> params = List.nil();
		List<JCExpression> thrown = List.nil();


		JCStatement[] tryStatementsArray = new JCStatement[1];
		
		JCIdent thisReceiver = maker.Ident(typeNode.toName("this"));
		
		JCMethodInvocation callGetClass = maker.Apply(
				List.<JCExpression>nil(),
				maker.Select(thisReceiver, typeNode.toName("getClass")), 
				List.<JCExpression>nil());

		JCMethodInvocation callName = maker.Apply(
				List.<JCExpression>nil(),
				maker.Select(thisReceiver, typeNode.toName("name")), 
				List.<JCExpression>nil());
		
		JCMethodInvocation callGetField = maker.Apply(
				List.<JCExpression>nil(),
				maker.Select(callGetClass, typeNode.toName("getField")), 
				List.<JCExpression>of(callName));
		
		
		JCIdent sopletClassLiteral = maker.Ident(typeNode.toName(SOPLET + ".class"));

		JCMethodInvocation callGetAnnotation = maker.Apply(
				List.<JCExpression>nil(),
				maker.Select(callGetField, typeNode.toName("getAnnotation")), 
				List.<JCExpression>of(sopletClassLiteral));

		JCMethodInvocation callGetAttributeValue = maker.Apply(
				List.<JCExpression>nil(),
				maker.Select(callGetAnnotation, typeNode.toName(methodDescriptor.methodName)), 
				List.<JCExpression>nil());
		
		tryStatementsArray[0] = maker.Return(callGetAttributeValue);
		List<JCStatement> tryStatementList = List.from(tryStatementsArray);
		
		
		
		JCExpression annotationValue = null;
		String returnTypeName = methodDescriptor.returnType.toString();
		if ("boolean".equals(returnTypeName)) {
			annotationValue = maker.Ident(typeNode.toName("false"));
		} else if ("byte".equals(returnTypeName)
				|| "char".equals(returnTypeName)
				|| "double".equals(returnTypeName)
				|| "float".equals(returnTypeName)
				|| "int".equals(returnTypeName)
				|| "long".equals(returnTypeName)
				|| "short".equals(returnTypeName)) {
			annotationValue = maker.Literal(0);
		} else {
			annotationValue = maker.Ident(typeNode.toName("null"));
		}
		JCReturn catchBody = maker.Return(annotationValue);

		JCStatement tryCatchBlock = buildTryCatchBlock(typeNode, tryStatementList, "java.lang.Exception", catchBody);
		
		List<JCStatement> mainStatementList = List.of(tryCatchBlock);
		JCBlock block = maker.Block(0, mainStatementList);
			 
		JCMethodDecl methodDecl = maker.MethodDef(mods, name, methodDescriptor.returnType, typarams, params, thrown, block, defaultValue);
		JCClassDecl parentEnum = typeDecl;
		parentEnum.defs = parentEnum.defs.append(methodDecl);
		typeNode.add(methodDecl, Kind.METHOD);

	}	
	
	private JCStatement buildTryCatchBlock(JavacNode node, List<JCStatement> contents, String exception, JCStatement catchBody) {
		TreeMaker maker = node.getTreeMaker();
		JCBlock tryBlock = maker.Block(0, contents);
		
		JCExpression varType = chainDots(node, exception.split("\\."));
		
		JCVariableDecl catchParam = maker.VarDef(maker.Modifiers(Flags.FINAL), node.toName("ex"), varType, null);

		List<JCStatement> blockStatements = List.of(catchBody);
		JCBlock catchBlock = maker.Block(0, blockStatements);

		
		return maker.Try(tryBlock, List.of(maker.Catch(catchParam, catchBlock)), null);
	}

	
//	private void createAccessorMethod(JCClassDecl typeDecl, JavacNode typeNode ) throws CoreException, TypeNotConvertibleException  {
//		
//		for (JavacNode enumEntryNode : typeNode.down()) {
//			//find the @Soplet annotation
//			for (JavacNode child : enumEntryNode.down()) {
//				if (child.getKind() != Kind.ANNOTATION) continue;
//				
//				JCAnnotation entryAnnotation = (JCAnnotation)child.get();
//				for (JCExpression argument : entryAnnotation.getArguments()) {
//					JCAssign assignment = (JCAssign)argument;
//					JCLiteral value = (JCLiteral)assignment.getExpression();
//					JCExpression variable = assignment.getVariable();
//					System.out.println(variable);
//					
//					TreeMaker maker = typeNode.getTreeMaker();
//					Class returnTypeClass = String.class; // method.getReturnType();
//					JCExpression jcReturnType = SopUtil.findJCTypeByFQName(typeNode.getAst(), returnTypeClass.getName());
//					
//					JCModifiers mods = maker.Modifiers(Flags.PUBLIC);
//					Name name = Table.instance(typeNode.getContext()).fromString("blubb");
//					List<JCTypeParameter> typarams = List.nil();
//					List<JCVariableDecl> params = List.nil();
//					List<JCExpression> thrown = List.nil();
//					JCBlock block = null;
//					
//					JCMethodDecl methodDecl = maker.MethodDef(mods, name, jcReturnType, typarams, params, thrown, block, null);
//
//					enumEntryNode.add(methodDecl, Kind.METHOD);
//					JCVariableDecl classDecl = (JCVariableDecl)enumEntryNode.get();
//					JCIdent ttype = (JCIdent)classDecl.vartype;
//					System.out.println(ttype);
//					//.defs.append(methodDecl);
//				}				
//			}
//		}
//	}	
	
	class MethodDescriptor {
		JCExpression returnType;
		String methodName;
		JCExpression defaultValue;
		
	}

}
