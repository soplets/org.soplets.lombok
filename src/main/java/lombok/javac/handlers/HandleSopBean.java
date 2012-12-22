package lombok.javac.handlers;

import static lombok.javac.handlers.JavacHandlerUtil.chainDots;

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.annotation.Generated;

import lombok.AccessLevel;
import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.javac.JavacAnnotationHandler;
import lombok.javac.JavacNode;
import lombok.javac.handlers.HandleGetter;
import lombok.javac.handlers.HandleSetter;
import lombok.javac.handlers.JavacHandlerUtil;
import lombok.soplets.SopBean;

import org.mangosdk.spi.ProviderFor;

import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.comp.Enter;
import com.sun.tools.javac.comp.Env;
import com.sun.tools.javac.jvm.ClassReader;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.JCTree.JCAnnotation;
import com.sun.tools.javac.tree.JCTree.JCAssign;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCFieldAccess;
import com.sun.tools.javac.tree.JCTree.JCIdent;
import com.sun.tools.javac.tree.JCTree.JCLiteral;
import com.sun.tools.javac.tree.JCTree.JCModifiers;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;

@Generated("soplet/lombok")
@ProviderFor(JavacAnnotationHandler.class)
public class HandleSopBean extends JavacAnnotationHandler<SopBean> {

	@Override
	public void handle(AnnotationValues<SopBean> annotation, JCAnnotation annotationAstNode, JavacNode annotationNode) {
		
		//deleteAnnotationIfNeccessary(annotationNode, SopBean.class);

		JavacNode typeNode = annotationNode.directUp();
		if (!(typeNode.get() instanceof JCClassDecl)) {
			return;
		}
		JCClassDecl typeClassDecl = (JCClassDecl) typeNode.get();
		
		//if @Generated then skip
		
		Iterator<JCAnnotation> it = typeClassDecl.mods.annotations.iterator();
		while (it.hasNext()) {
			JCAnnotation ann = it.next();
			String name = ann.type.toString(); 
			if (name.indexOf("Generated") > 0) {
				return;
			}
		}
		
		try {			
			String sopletRefName = annotation.getProbableFQType("sopRef");
			if (sopletRefName == null || sopletRefName.length() == 0) {
				annotationNode.addError("No sopRef value available");
				return;
			}
			
			Context context = typeNode.getContext();
			ClassReader reader = ClassReader.instance(context);
			
			//Name flatname = Table.instance(context).fromString(sopletRefName);
			Name flatname = Names.instance(context).fromString(sopletRefName);
			
			ClassSymbol symbol = reader.loadClass(flatname);
			Enter enter = Enter.instance(context);
			Env<?> o3 = enter.getEnv(symbol);
			if (o3 == null) return;			
			JCClassDecl classDecl = o3.enclClass;
			
			for (JCTree def : classDecl.defs ) {
				
				if (!(def instanceof JCVariableDecl)) continue;
				JCVariableDecl entry = (JCVariableDecl)def;
				
				//find Soplet annotation
				JCAnnotation sopletAnnotation = null;
				List<JCAnnotation> annotations = entry.getModifiers().getAnnotations();
				
				List<JCAnnotation> otherAnnotations = new Vector<JCTree.JCAnnotation>();
				for (JCAnnotation entryAnnotation : annotations) {
					JCIdent ident = (JCIdent)entryAnnotation.getAnnotationType();
					if ("Soplet".equals(ident.getName().toString())) {
						sopletAnnotation = entryAnnotation;
					} else {
						otherAnnotations.add(entryAnnotation);
					}
				}
				if (sopletAnnotation == null) {
					//entry without @Soplet annotation, skip it...
					continue;
				}
				
				//find the javaType
				JCAssign assignment = null; 
				List<JCExpression> arguments = sopletAnnotation.getArguments();
				for (JCExpression argument : arguments) {
					if (!(argument instanceof JCAssign)) {
						continue;
					}
					JCAssign assign = (JCAssign)argument;
					JCIdent varName = (JCIdent)assign.getVariable();
					if ("javaType".equals(varName.getName().toString())) {
						assignment = assign;
						break;
					}
					
				}
				TreeMaker maker = typeNode.getTreeMaker();
				JCExpression classIdent2 = null;
				if (assignment != null) {
					JCFieldAccess classIdent = (JCFieldAccess)assignment.getExpression();
					classIdent2 = classIdent.getExpression(); 
				} else {
					classIdent2 = maker.Ident(typeNode.getAst().toName("String"));
				}

				
				//add attribute
				Name name = entry.getName();
				JCModifiers mods = maker.Modifiers(Flags.PRIVATE);
				JCVariableDecl vd = maker.VarDef(mods, name, classIdent2, null);
				
				@SuppressWarnings("unused")
				JavacNode fieldNode = typeNode.add(vd, Kind.FIELD);
				JavacHandlerUtil.injectField(typeNode, vd);
				
//				long access = Flags.PUBLIC;
				
				//JCMethodDecl mdSetter = new HandleSetter().createSetter(access, fieldNode, maker, typeNode.get());
				
//				typeClassDecl.defs = typeClassDecl.defs.append(mdSetter);
//				typeNode.add(mdSetter, Kind.METHOD);
								
//				JCMethodDecl mdGetter = new HandleGetter().createGetter(access, fieldNode, maker, false, typeNode.get());
//				typeClassDecl.defs = typeClassDecl.defs.append(mdGetter);

				new HandleGetter().generateGetterForType(typeNode, annotationNode, AccessLevel.PUBLIC, true);
				new HandleSetter().generateSetterForType(typeNode, annotationNode, AccessLevel.PUBLIC, true);
				
				//copy other annotations to getter method
//				com.sun.tools.javac.util.List<JCAnnotation> annotationList = 
//						com.sun.tools.javac.util.List.from(otherAnnotations.toArray(new JCAnnotation[0]));
//				mdGetter.getModifiers().annotations = annotationList;
				

				//				//do we need this?
				//				JavacNode getterNode = typeNode.add(mdGetter, Kind.METHOD);
				//				for (JCAnnotation otherAnnotation : otherAnnotations) {
				//					getterNode.add(otherAnnotation, Kind.ANNOTATION);
				//				}				
			}

			//			//add @lombok.soplets.SopBeanRef(SopXY.class) annotation
			//			TreeMaker maker = typeNode.getTreeMaker();
			//			JCExpression annotationExpression = chainDots(typeNode, "lombok", "soplets", "SopBeanRef");
			//			JCIdent runtimeValue = maker.Ident(typeNode.toName(sopletRefName + ".class"));
			//			JCAnnotation annotationBeanRef = maker.Annotation(annotationExpression, com.sun.tools.javac.util.List.<JCExpression>of(runtimeValue)); 
			//			typeClassDecl.mods.annotations = typeClassDecl.mods.annotations.append(annotationBeanRef);

			//add @Generated("soplet/lombok")
			TreeMaker maker = typeNode.getTreeMaker();
			JCExpression annotationExpression = chainDots(typeNode, "javax", "annotation", "Generated");
			JCLiteral runtimeValue = maker.Literal("soplet/lombok");
			JCAnnotation annotationBeanRef = maker.Annotation(annotationExpression, com.sun.tools.javac.util.List.<JCExpression>of(runtimeValue)); 
			typeClassDecl.mods.annotations = typeClassDecl.mods.annotations.append(annotationBeanRef);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
