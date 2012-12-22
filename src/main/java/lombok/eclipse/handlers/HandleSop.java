package lombok.eclipse.handlers;

import static lombok.eclipse.Eclipse.ECLIPSE_DO_NOT_TOUCH_FLAG;
import static lombok.eclipse.Eclipse.fromQualifiedName;
import static lombok.eclipse.Eclipse.pos;
import static lombok.eclipse.Eclipse.poss;
import static lombok.eclipse.handlers.EclipseHandlerUtil.copyType;
import static lombok.eclipse.handlers.EclipseHandlerUtil.makeIntLiteral;
import static lombok.eclipse.handlers.EclipseHandlerUtil.makeMarkerAnnotation;
import static lombok.eclipse.handlers.EclipseHandlerUtil.setGeneratedBy;

import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.eclipse.EclipseAnnotationHandler;
import lombok.eclipse.EclipseNode;
import lombok.soplets.Sop;

import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.AnnotationMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.Argument;
import org.eclipse.jdt.internal.compiler.ast.Block;
import org.eclipse.jdt.internal.compiler.ast.ClassLiteralAccess;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.FalseLiteral;
import org.eclipse.jdt.internal.compiler.ast.MemberValuePair;
import org.eclipse.jdt.internal.compiler.ast.MessageSend;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.NormalAnnotation;
import org.eclipse.jdt.internal.compiler.ast.NullLiteral;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedSingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedNameReference;
import org.eclipse.jdt.internal.compiler.ast.ReturnStatement;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Statement;
import org.eclipse.jdt.internal.compiler.ast.ThisReference;
import org.eclipse.jdt.internal.compiler.ast.TryStatement;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.ast.Wildcard;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(EclipseAnnotationHandler.class)
public class HandleSop extends EclipseAnnotationHandler<Sop> {

	private final static char[] SOPLET = "Soplet".toCharArray();
	
	private int sourceStart;
	private int sourceEnd;
	long p;
	
	@Override
	public void handle(	AnnotationValues<Sop> annotation, Annotation ast, EclipseNode annotationNode) {
		EclipseNode typeNode = annotationNode.up();

		ASTNode source = annotationNode.get();
		sourceStart = source.sourceStart;
		sourceEnd = source.sourceEnd;
		p = pos(source);
		
		if (notAnEnum(typeNode)) {
			annotationNode.addError("@Sop is only supported on an enum");
			return;
		}
		if (!(typeNode.get() instanceof TypeDeclaration)) {
			return;
		}
		
		TypeDeclaration typeDecl = (TypeDeclaration) typeNode.get();

		try {		
			List<String> aspectNames = annotation.getProbableFQTypes("aspects");
			if (aspectNames == null || aspectNames.size() == 0) {
				annotationNode.addError("Invalid stereotype");
				return;
			}

			Hashtable<String, MethodBean> methodBeans = new Hashtable<String, MethodBean>();
			List<IType> aspectTypes = new Vector<IType>();
			
			boolean b = true;
			if (b) {
				for (String aspectName : aspectNames) {
					IType aspectType = SopUtilEcj.findTypeByFQName(aspectName);
					
					if (aspectType == null) {
						continue;
					}
					
					aspectTypes.add(aspectType);
					for (IMethod method : aspectType.getMethods()) {
						String name = method.getElementName();
						if (methodBeans.contains(name)) {
							continue;
						}
						
						MethodBean methodBean = new MethodBean();
						methodBean.name = name;
						methodBean.returnType = method.getReturnType();
						methodBean.defaultValue = method.getDefaultValue();
						methodBeans.put(name, methodBean);
					}
				}
			} else {
				//just for testing!!
				MethodBean methodBean = new MethodBean();
				methodBean.name = "textEN";
				methodBean.returnType = "LObject;";
				methodBeans.put(methodBean.name, methodBean);
			}
			
			if (methodBeans.size() == 0) return;  //TODO throw error

			HashSet<String> attributeNames = new HashSet<String>();
			List<MethodDeclaration> attributeDeclarations = new Vector<MethodDeclaration>();
			for (MethodBean method : methodBeans.values()) {
				attributeNames.add(method.name);
				TypeReference typeReference = SopUtilEcj.parseTypeFromSignature(method.returnType, source);
				
				//create getter methods
				MethodDeclaration getterMethod = createGetter(typeDecl, typeReference, method, source);
				EclipseHandlerUtil.injectMethod(typeNode, getterMethod);
			
				//prepare the @Soplet's attributes
				MethodDeclaration md = createAnnotationAttribute(typeDecl, typeReference, source, method);
//				MethodDeclaration md = createAnnotationAttribute(typeDecl, typeReference, method, ast);
				attributeDeclarations.add(md);
			}
			
			//add @Soplet inner class declaration 
			MethodDeclaration[] mds = attributeDeclarations.toArray(new MethodDeclaration[attributeDeclarations.size()]);
			TypeDeclaration sopAnnotation = createSopInterface(typeDecl, source, mds);
			injectInterfaceDeclaration(typeNode, sopAnnotation);
			
			//add "implements"
			addImplementors(typeNode, aspectTypes, source);
			
			addAnnotationTypeMethod(typeNode, source);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	//add "implements " + aspect interfaces
	private void addImplementors(EclipseNode typeNode, List<IType> stereoTypes, ASTNode source) {		
		TypeDeclaration	typeDecl = (TypeDeclaration) typeNode.get();
		TypeReference[] interfaces = null;
		if (typeDecl.superInterfaces == null || typeDecl.superInterfaces.length == 0) {
			interfaces = new TypeReference[stereoTypes.size()];
		} else {
			interfaces = new TypeReference[stereoTypes.size() + typeDecl.superInterfaces.length];
			System.arraycopy(typeDecl.superInterfaces, 0, interfaces, stereoTypes.size(), typeDecl.superInterfaces.length);
		}
		int i = 0;
		
		for (IType stereoType : stereoTypes) {
			String stereoTypeName = stereoType.getElementName();
			SingleTypeReference annotationClassReference = new SingleTypeReference(stereoTypeName.toCharArray(), p);
			annotationClassReference.sourceStart = typeDecl.sourceStart;
			annotationClassReference.sourceEnd = typeDecl.sourceEnd;

			interfaces[i++] = annotationClassReference;
			setGeneratedBy(annotationClassReference, source);
		}
		typeDecl.superInterfaces = interfaces;
	}
	
	/**
	 * Add annotationType method (required to apeace the compiler...)
	 * 
	 * @Override
	 * public Class<? extends Annotation> annotationType() {
	 *   return Soplet.class;
	 * }
	 */
	private void addAnnotationTypeMethod(EclipseNode typeNode, ASTNode source) {
		TypeDeclaration typeDecl = (TypeDeclaration) typeNode.get();
		MethodDeclaration method = new MethodDeclaration(typeDecl.compilationResult);
		setGeneratedBy(method, source);
		
		// @Override
		method.annotations = new Annotation[] {makeMarkerAnnotation(TypeConstants.JAVA_LANG_OVERRIDE, source)};
		method.modifiers = Modifier.PUBLIC;
		method.typeParameters = null;
		
		// <? extends java.lang.annotation.Annotation>
		Wildcard wildcard = new Wildcard(Wildcard.EXTENDS);
		setGeneratedBy(wildcard, source);
		
//		char[][] elems = fromQualifiedName("java.lang.annotation.Annotation");
//		long[] poss = new long[elems.length];
//		Arrays.fill(poss, p);
//		wildcard.bound = new QualifiedTypeReference(elems, poss);
		
		wildcard.bound = SopUtilEcj.createQualifiedTypeReference("java.lang.annotation.Annotation", source);
		setGeneratedBy(wildcard.bound, source);
		wildcard.sourceStart = wildcard.bound.sourceStart = sourceStart;
		wildcard.sourceEnd = wildcard.bound.sourceEnd = sourceEnd;
		
		
		// Class<? extends java.lang.annotation.Annotation>
		TypeReference classReference = new ParameterizedSingleTypeReference("Class".toCharArray(), new TypeReference[] { wildcard }, 0, p);
		setGeneratedBy(classReference, source);

		TypeReference returnType = copyType(classReference, source);
		
		method.returnType = returnType;
		method.selector = "annotationType".toCharArray();
		method.arguments = null;
		method.binding = null;
		method.thrownExceptions = null;
		method.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
		
		// Soplet.class
		TypeReference expr = new SingleTypeReference(SOPLET, p);
		setGeneratedBy(expr, source);
		ClassLiteralAccess dotClass = new ClassLiteralAccess(sourceEnd, expr);
		setGeneratedBy(dotClass, source);
		
		// return Soplet.class;
		Statement returnDefaultStatement = new ReturnStatement(dotClass, sourceStart, sourceEnd);
		setGeneratedBy(returnDefaultStatement, source);			
		method.statements = new Statement[] {returnDefaultStatement};
		method.bodyStart = method.declarationSourceStart = method.sourceStart = sourceStart;
		method.bodyEnd = method.declarationSourceEnd = method.sourceEnd = sourceEnd;
		
		EclipseHandlerUtil.injectMethod(typeNode, method);		
	}
	
	/**
	 * Creates an accessor method, something like:
	 * 
	 * private String textEN() {
	 *   try {
	 *     return getClass().getField(name()).getAnnotation(Soplet.class).textEN();
	 *   } catch (Exception e) {
	 *   }
	 *   return null;
	 * }
	 */
	private MethodDeclaration createGetter(TypeDeclaration typeDecl, TypeReference typeReference, MethodBean annotationMethod, ASTNode source) throws JavaModelException {
		char[] attributeName = annotationMethod.name.toCharArray();
		MethodDeclaration method = new MethodDeclaration(typeDecl.compilationResult);
		setGeneratedBy(method, source);
		method.annotations = null;
		method.modifiers = Modifier.PUBLIC;
		method.typeParameters = null;		
		TypeReference returnType = copyType(typeReference, source);
		setStartEnd(returnType);
		method.returnType = returnType;
		method.selector = attributeName;
		method.arguments = null;
		method.binding = null;
		method.thrownExceptions = null;
		method.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
		method.bodyStart = method.declarationSourceStart = method.sourceStart = sourceStart;
		method.bodyEnd = method.declarationSourceEnd = method.sourceEnd = sourceEnd;

		// In org.eclipse.jdt.core.dom.ASTConverter#convert(org.eclipse.jdt.internal.compiler.ast.MessageSend)
		// a new SimpleName is created and the setSourceRange is 0, 1. Nothing I know can fix that.
		// Maybe experimenting with the field MessageSend.nameSourcePosition, but I couldn't make progress 
		
		//getClass()
		MessageSend callToGetClass = new MessageSend();
		setMessageStartEnd(callToGetClass);
		callToGetClass.receiver = ThisReference.implicitThis();
		setStartEnd(callToGetClass.receiver);
		callToGetClass.selector = "getClass".toCharArray();	
		setGeneratedBy(callToGetClass, source);
		
		//name()
		MessageSend callToName = new MessageSend();
		setMessageStartEnd(callToName);
		callToName.receiver = ThisReference.implicitThis();
		callToName.selector = "name".toCharArray();	
		setGeneratedBy(callToName, source);
		
		//.getField(name())
		MessageSend callToGetField = new MessageSend();
		setMessageStartEnd(callToGetField);
		callToGetField.receiver = callToGetClass;
		callToGetField.selector = "getField".toCharArray();		
		callToGetField.arguments = new Expression[] { callToName };
		setGeneratedBy(callToGetField, source);
		
		//Soplet.class
		TypeReference annotationClassReference = new SingleTypeReference(SOPLET, p);
		setGeneratedBy(annotationClassReference, source);
		ClassLiteralAccess dotClass = new ClassLiteralAccess(sourceEnd, annotationClassReference);
		setGeneratedBy(dotClass, source);
		
		//getAnnotation(Soplet.class)
		MessageSend callToGetAnnotation = new MessageSend();
		setMessageStartEnd(callToGetAnnotation);
		callToGetAnnotation.receiver = callToGetField;
		callToGetAnnotation.selector = "getAnnotation".toCharArray();
		callToGetAnnotation.arguments = new Expression[] { dotClass };
		setGeneratedBy(callToGetAnnotation, source);
		
		//.nameOfField()
		MessageSend callToDescription = new MessageSend();
		setMessageStartEnd(callToDescription);
		callToDescription.receiver = callToGetAnnotation;
		callToDescription.selector = attributeName;
		setGeneratedBy(callToDescription, source);
		
		Statement returnStatement = new ReturnStatement(callToDescription, sourceStart, sourceEnd);
		setGeneratedBy(returnStatement, source);

		TryStatement tryStatement = new TryStatement();
		setGeneratedBy(tryStatement, source);
		setStartEnd(tryStatement);

		tryStatement.tryBlock = new Block(0);
		setGeneratedBy(tryStatement.tryBlock, source);
		setStartEnd(tryStatement.tryBlock);

		tryStatement.tryBlock.statements = new Statement[] {returnStatement };

		TypeReference exceptionReference = new SingleTypeReference("Exception".toCharArray(), p);
		typeReference.statementEnd = sourceEnd;
		setGeneratedBy(exceptionReference, source);
		
		Argument catchArg = new Argument("ex".toCharArray(), 0, exceptionReference, Modifier.FINAL);
		// In AST the CatchClause starts 3 characters after sourceStart (couldn't figure out how to fix it)
		// If sourceStart is used, then it the IDE correctly selects @Sop (but the AST comes out with an error)
		catchArg.declarationSourceStart = catchArg.modifiersSourceStart = catchArg.sourceStart = sourceEnd;
		catchArg.declarationSourceEnd = catchArg.declarationEnd = catchArg.sourceEnd = sourceEnd;
		setGeneratedBy(catchArg, source);
		
		tryStatement.catchArguments = new Argument[] { catchArg };
		tryStatement.catchBlocks = new Block[1];
		tryStatement.catchBlocks[0] = new Block(0);
		// In AST the CatchClause starts 3 characters after sourceStart (couldn't figure out how to fix it)
		// If sourceStart is used, then it the IDE correctly selects @Sop (but the AST comes out with an error)
		tryStatement.catchBlocks[0].sourceStart = sourceEnd;
		tryStatement.catchBlocks[0].sourceEnd = sourceEnd;
		setGeneratedBy(tryStatement.catchBlocks[0], source);

		Expression expr = null;
		char[] signature = annotationMethod.returnType.toCharArray();
		switch (signature[0]) {
		case Signature.C_BOOLEAN:
			expr = new FalseLiteral(sourceStart, sourceEnd);
			break;
		case Signature.C_INT:
		case Signature.C_LONG:
		case Signature.C_DOUBLE:
		case Signature.C_FLOAT:
			expr = makeIntLiteral(new char[] { '0' }, source);
			break;
		default:
			expr = new NullLiteral(sourceStart, sourceEnd);
		}
		Statement returnDefaultStatement = new ReturnStatement(expr, sourceStart, sourceEnd);
		setGeneratedBy(returnDefaultStatement, source);			

		method.statements = new Statement[] { tryStatement, returnDefaultStatement};
		return method;
	}

	private void setStartEnd(ASTNode node) {
		node.sourceStart = sourceStart;
		node.sourceEnd = sourceEnd;
	}
	
	private void setMessageStartEnd(MessageSend msg) {
		setStartEnd(msg);
		msg.statementEnd = sourceEnd;
	}
	
	private boolean notAnEnum(EclipseNode typeNode) {
		TypeDeclaration typeDecl = null;
		if (typeNode.get() instanceof TypeDeclaration) {
			typeDecl = (TypeDeclaration) typeNode.get();
		}
		int modifiers = typeDecl == null ? 0 : typeDecl.modifiers;
		return typeDecl != null &&
		   (modifiers & (ClassFileConstants.AccInterface | ClassFileConstants.AccAnnotation | ClassFileConstants.AccDefault)) != 0;
	}

	public static void injectInterfaceDeclaration(EclipseNode type, TypeDeclaration interfaceDeclaration) {
		TypeDeclaration parent = (TypeDeclaration) type.get();		
		if (parent.memberTypes == null) {
			parent.memberTypes = new TypeDeclaration[1];
			parent.memberTypes[0] = interfaceDeclaration;
		} else {
			TypeDeclaration[] newArray = new TypeDeclaration[parent.memberTypes.length + 1];
			System.arraycopy(parent.memberTypes, 0, newArray, 0, parent.memberTypes.length);
			newArray[parent.memberTypes.length] = interfaceDeclaration;
			parent.memberTypes = newArray;
		}		
		type.add(interfaceDeclaration, Kind.TYPE);
	}
	
	private TypeDeclaration createSopInterface(TypeDeclaration typeDecl, ASTNode source, MethodDeclaration[] attributes) {
		TypeDeclaration annotationTypeDeclaration = new TypeDeclaration(typeDecl.compilationResult);		
		annotationTypeDeclaration.modifiers = ClassFileConstants.AccPublic;
		annotationTypeDeclaration.name = SOPLET;
		annotationTypeDeclaration.modifiers = ClassFileConstants.AccInterface|ClassFileConstants.AccAnnotation;
		annotationTypeDeclaration.methods = attributes;
		annotationTypeDeclaration.bodyStart = annotationTypeDeclaration.declarationSourceStart = sourceStart;
		annotationTypeDeclaration.bodyEnd = annotationTypeDeclaration.declarationSourceEnd = sourceEnd;
		annotationTypeDeclaration.sourceStart = sourceStart;
		annotationTypeDeclaration.sourceEnd = sourceEnd;
		
		setGeneratedBy(annotationTypeDeclaration, source);
		
		//add @Retention(RetentionPolicy.RUNTIME)
		TypeReference annotationClassReference = SopUtilEcj.createQualifiedTypeReference("java.lang.annotation.Retention", source);
		NormalAnnotation ann = new NormalAnnotation(annotationClassReference, sourceStart);
		ann.declarationSourceEnd = ann.statementEnd = ann.sourceEnd = sourceEnd;
		setGeneratedBy(ann, source);
		
		//RetentionPolicy.RUNTIME
		char[][] typeNameTokens = fromQualifiedName("java.lang.annotation.RetentionPolicy.RUNTIME");
		QualifiedNameReference qnr = new QualifiedNameReference(typeNameTokens, poss(source, typeNameTokens.length), sourceStart, sourceEnd);
		setGeneratedBy(qnr, source);
		
		MemberValuePair mvp = new MemberValuePair("value".toCharArray(), sourceStart, sourceEnd, qnr);
		ann.memberValuePairs = new MemberValuePair[]{mvp};
		setGeneratedBy(mvp, source);
		
		annotationTypeDeclaration.annotations = new Annotation[]{ann};
		return annotationTypeDeclaration;		
	}

	/**
	 * create a new attribute for the inner-class Soplet @interface
	 */
	private MethodDeclaration createAnnotationAttribute(TypeDeclaration typeDecl, TypeReference typeReference, ASTNode source, MethodBean attributeMethod) throws JavaModelException {
		AnnotationMethodDeclaration method = new AnnotationMethodDeclaration(typeDecl.compilationResult);
		TypeReference returnType = copyType(typeReference, source);
		setStartEnd(returnType);
		method.returnType = returnType;
		method.selector = attributeMethod.name.toCharArray();
		method.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
		method.statements = null;
		method.bodyStart = method.declarationSourceStart = method.sourceStart = sourceStart;
		method.bodyEnd = method.declarationSourceEnd = method.sourceEnd = sourceEnd;

		IMemberValuePair defaultValue = attributeMethod.defaultValue;
		if (defaultValue == null) {
			method.modifiers = Modifier.ABSTRACT;
		} else {
			method.modifiers = ClassFileConstants.AccAnnotationDefault;
			Expression expr = SopUtilEcj.extractValueFromAnnotation(defaultValue, source);
			setGeneratedBy(expr, source);
			method.defaultValue = expr;
		}
		setGeneratedBy(method, source);
		return method;
	}
	
	class MethodBean {
		public IMemberValuePair defaultValue;
		public String returnType;
		public String name;
		
	}
}