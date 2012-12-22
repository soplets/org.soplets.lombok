package lombok.eclipse.handlers;

import static lombok.eclipse.Eclipse.ECLIPSE_DO_NOT_TOUCH_FLAG;
import static lombok.eclipse.handlers.EclipseHandlerUtil.setGeneratedBy;

import java.lang.reflect.Modifier;

import lombok.AccessLevel;
import lombok.core.AnnotationValues;
import lombok.eclipse.EclipseAnnotationHandler;
import lombok.eclipse.EclipseNode;
import lombok.soplets.SopBean;

import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.MemberValuePair;
import org.eclipse.jdt.internal.compiler.ast.NormalAnnotation;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(EclipseAnnotationHandler.class)
public class HandleSopBean extends EclipseAnnotationHandler<SopBean> {

	@Override
	public void handle(AnnotationValues<SopBean> annotation, Annotation ast, EclipseNode annotationNode) {
		EclipseNode typeNode = annotationNode.up();
		if(notAClass(typeNode)) {
			annotationNode.addError("@SopBean is only supported on a class.");
			return;
		}
		
		try {			
			String soplotName = annotation.getProbableFQType("sopRef");
			if (soplotName == null) {
				annotationNode.addError("No valid type attribute defined");
				return;
			}
			IType sopletType = SopUtilEcj.findTypeByFQName(soplotName);
			if (sopletType == null) return;  //TODO throw error

			if (!( typeNode.get() instanceof TypeDeclaration)) {
				return;
			}

			TypeDeclaration typeDecl = (TypeDeclaration) typeNode.get();
			for (IField f : sopletType.getFields()) {
				IAnnotation sopletAnnotation = f.getAnnotation("Soplet");
				if (sopletAnnotation == null) {
					continue;
				}
				IMemberValuePair[] mvPairs = sopletAnnotation.getMemberValuePairs();
				Object javaType = "String";
				for (IMemberValuePair mvPair : mvPairs) {
					if ("javaType".equals(mvPair.getMemberName())) {
						javaType = mvPair.getValue();
						break;
					}
				}
				
				String name = f.getElementName();
				FieldDeclaration fieldDecl = createAttribute(typeDecl, annotationNode.get(), name, javaType.toString());
				EclipseHandlerUtil.injectField(typeNode, fieldDecl);

//				EclipseNode fieldNode = typeNode.getNodeFor(fieldDecl);

				//add annotations
				addAnnotationsToField(f, fieldDecl, annotationNode.get());

//				MethodDeclaration getterMethod = createGetter(
//						typeDecl, 
//						fieldNode, 
//						annotationAstNode,
//						"boolean".equalsIgnoreCase(javaType.toString()));
//				EclipseHandlerUtil.injectMethod(typeNode, getterMethod);
//
//
//				MethodDeclaration setterMethod = generateSetter(
//						typeDecl, 
//						fieldNode, 
//						annotationAstNode);
//				EclipseHandlerUtil.injectMethod(typeNode, setterMethod);
//				
				new HandleGetter().generateGetterForType(typeNode, annotationNode, AccessLevel.PUBLIC, true);
				new HandleSetter().generateSetterForType(typeNode, annotationNode, AccessLevel.PUBLIC, true);

			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void addAnnotationsToField(IField sourceField, FieldDeclaration fieldDecl, ASTNode source) {
		
		try {
			int i = 0;
			IAnnotation[] sopletAnnotations = sourceField.getAnnotations();
			NormalAnnotation[] normalAnnotations = new NormalAnnotation[sopletAnnotations.length -1]; 
			for (IAnnotation ann : sopletAnnotations) {
				if ("Soplet".equals(ann.getElementName())) continue;
								
				int sourceStart = source.sourceStart;
				int sourceEnd = source.sourceEnd;
				//TODO fully qualify!
//				char[][] typeNameTokens = fromQualifiedName(ann.getElementName());
//				long[] pos = new long[typeNameTokens.length];
//				TypeReference annotationClassReference = new QualifiedTypeReference(typeNameTokens, pos);
				
				TypeReference annotationClassReference = new SingleTypeReference(ann.getElementName().toCharArray(), sourceStart);
				normalAnnotations[i] = new NormalAnnotation(annotationClassReference, sourceStart);
				normalAnnotations[i].declarationSourceEnd = normalAnnotations[i].statementEnd = normalAnnotations[i].sourceEnd = sourceEnd;
				setGeneratedBy(normalAnnotations[i], source);
				
				int j = 0;
				MemberValuePair[] targetMvp = new MemberValuePair[ann.getMemberValuePairs().length];
				for (IMemberValuePair sourceMvp : ann.getMemberValuePairs()) {
					Expression expr = SopUtilEcj.extractValueFromAnnotation(sourceMvp, source);
					setGeneratedBy(expr, source);
					targetMvp[j] = new MemberValuePair(sourceMvp.getMemberName().toCharArray(), sourceStart, sourceEnd, expr);
					setGeneratedBy(targetMvp[j++], source);
				}
				normalAnnotations[i].memberValuePairs = targetMvp;
				i++;
			}
			fieldDecl.annotations = normalAnnotations;				
			
		} catch (JavaModelException e) {
			e.printStackTrace();
		}
	}

//	private MethodDeclaration generateSetter(TypeDeclaration parent, EclipseNode fieldNode, Annotation sourceAnnotation) {
//		FieldDeclaration field = (FieldDeclaration) fieldNode.get();
//		//int pS = source.sourceStart, pE = source.sourceEnd;
//		//long p = (long)pS << 32 | pE;
//		MethodDeclaration method = new MethodDeclaration(parent.compilationResult);
//		setGeneratedBy(method, sourceAnnotation);
//		method.modifiers = Modifier.PUBLIC;
//		method.returnType = TypeReference.baseTypeReference(TypeIds.T_void, 0);
//		//method.returnType.sourceStart = pS; method.returnType.sourceEnd = pE;
//		setGeneratedBy(method.returnType, sourceAnnotation);
//		method.annotations = null;
//		Argument param = new Argument(field.name, sourceAnnotation.sourceStart, copyType(field.type, sourceAnnotation), Modifier.FINAL);
//		//param.sourceStart = pS; param.sourceEnd = pE;
//		setGeneratedBy(param, sourceAnnotation);
//		method.arguments = new Argument[] { param };
//		String name2 = "set" + fieldNode.getName().substring(0, 1).toUpperCase() + fieldNode.getName().substring(1);		
//		method.selector = name2.toCharArray();
//		method.binding = null;
//		method.thrownExceptions = null;
//		method.typeParameters = null;
//		method.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
//		
//		Expression fieldRef = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, sourceAnnotation);
//		NameReference fieldNameRef = new SingleNameReference(field.name, sourceAnnotation.sourceStart);
//		
//		setGeneratedBy(fieldNameRef, sourceAnnotation);
//		Assignment assignment = new Assignment(fieldRef, fieldNameRef, sourceAnnotation.sourceEnd);
//		//assignment.sourceStart = pS; assignment.sourceEnd = pE;
//		method.statements = new Statement[] { assignment };
//		setGeneratedBy(assignment, sourceAnnotation);
////		method.bodyStart = method.declarationSourceStart = method.sourceStart = source.sourceStart;
////		method.bodyEnd = method.declarationSourceEnd = method.sourceEnd = source.sourceEnd;		
//		return method;
//	}
//
//	
//	private MethodDeclaration createGetter(TypeDeclaration typeDecl, EclipseNode fieldNode, Annotation sourceAnnotation, boolean isOrGet) {
//		MethodDeclaration method = new MethodDeclaration(typeDecl.compilationResult);
//		setGeneratedBy(method, sourceAnnotation);
//		method.annotations = null;
//		method.modifiers = Modifier.PUBLIC;
//		method.typeParameters = null;
//		TypeReference returnType = copyType(((FieldDeclaration) fieldNode.get()).type, sourceAnnotation);
//		method.returnType = returnType;
//		String name2 = fieldNode.getName().substring(0, 1).toUpperCase() + fieldNode.getName().substring(1);
//		name2 = (isOrGet ? "is" : "get") + name2;   
//		method.selector = name2.toCharArray();
//		method.arguments = null;
//		method.binding = null;
//		method.thrownExceptions = null;
//		method.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
//		
//		//FieldDeclaration field = (FieldDeclaration) fieldNode.get();
//		Expression fieldRef = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, sourceAnnotation);
//		Statement returnStatement = new ReturnStatement(fieldRef, sourceAnnotation.sourceStart, sourceAnnotation.sourceEnd);
//		setGeneratedBy(returnStatement, sourceAnnotation);
//		
//		method.statements = new Statement[] { returnStatement };
//		return method;
//	}

	private FieldDeclaration createAttribute(TypeDeclaration typeDecl, ASTNode source, String name, String typeName) {
		
		FieldDeclaration field = new FieldDeclaration();
		setGeneratedBy(field, source);
		field.annotations = null;
		TypeReference attributeType = new SingleTypeReference(typeName.toCharArray(), source.sourceStart);
		field.type = attributeType;
		field.modifiers = Modifier.PRIVATE;
		field.name = name.toCharArray();
		field.binding = null;		
		field.bits |= ECLIPSE_DO_NOT_TOUCH_FLAG;
		
		//field.declarationSourceStart = field.sourceStart = astNode.sourceStart;
		//field.declarationSourceEnd = field.sourceEnd = astNode.sourceEnd;
		return field;
	}

	private boolean notAClass(EclipseNode typeNode) {
		TypeDeclaration typeDecl = null;
		if (typeNode.get() instanceof TypeDeclaration) typeDecl = (TypeDeclaration) typeNode.get();
		int modifiers = typeDecl == null ? 0 : typeDecl.modifiers;
		return typeDecl != null &&
		   (modifiers &	(ClassFileConstants.AccInterface | ClassFileConstants.AccAnnotation | ClassFileConstants.AccEnum)) != 0;
	}
}