package lombok.eclipse.handlers;

import static lombok.eclipse.Eclipse.fromQualifiedName;
import static lombok.eclipse.Eclipse.poss;
import static lombok.eclipse.Eclipse.pos;
import static lombok.eclipse.handlers.EclipseHandlerUtil.makeIntLiteral;
import static lombok.eclipse.handlers.EclipseHandlerUtil.setGeneratedBy;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ClassLiteralAccess;
import org.eclipse.jdt.internal.compiler.ast.DoubleLiteral;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.FalseLiteral;
import org.eclipse.jdt.internal.compiler.ast.QualifiedNameReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleNameReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.StringLiteral;
import org.eclipse.jdt.internal.compiler.ast.TrueLiteral;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;

public class SopUtilEcj {
	
	public static TypeReference parseTypeFromSignature(String returnType, ASTNode source) {
		int kind = Signature.getTypeSignatureKind(returnType);
		long p = pos(source);
		
		if (kind == Signature.BASE_TYPE_SIGNATURE) {			
			char[] simpleTypeName = Signature.toCharArray(returnType.toCharArray());
			
			return new SingleTypeReference(simpleTypeName, p);
		} else if (kind == Signature.ARRAY_TYPE_SIGNATURE) {
			String simpleTypeName = returnType.substring(2);  //cut leading '[Q'
			simpleTypeName = simpleTypeName.substring(0, simpleTypeName.length() - 1);  //cut trailing ';'
			
			return new ArrayTypeReference(simpleTypeName.toCharArray(), 1, p);
		} else if (returnType.indexOf('.') > 0) {
			returnType = returnType.substring(1);
			returnType = returnType.substring(0, returnType.length() - 1);
			char[][] typeNameTokens = fromQualifiedName(returnType);
			long[] poss = poss(source, typeNameTokens.length);				
			
			return new QualifiedTypeReference(typeNameTokens, poss);
		} else {
			returnType = returnType.substring(1);
			returnType = returnType.substring(0, returnType.length() - 1);
			
			return new SingleTypeReference(returnType.toCharArray(), p);
		}
	}	
	
	public static Expression extractValueFromAnnotation(IMemberValuePair defaultValue, ASTNode source) {
		int sourceStart = source.sourceStart;
		int sourceEnd = source.sourceEnd;
		String value = defaultValue.getValue().toString();
		switch (defaultValue.getValueKind()) {
			case IMemberValuePair.K_CLASS:
				TypeReference annotationClassReference  = null;
				if (value.indexOf('.')  == -1) {
					// Simple Name 
					annotationClassReference = new SingleTypeReference(value.toCharArray(), pos(source));
				} else {
					// Qualified
					annotationClassReference = createQualifiedTypeReference(value, source);
				}
				setGeneratedBy(annotationClassReference, source);
				
				return new ClassLiteralAccess(sourceEnd, annotationClassReference);
			case IMemberValuePair.K_QUALIFIED_NAME:
				char[][] qualifiedName = fromQualifiedName(value);
				return new QualifiedNameReference(qualifiedName, new long[2], sourceStart, sourceEnd);
			case IMemberValuePair.K_SIMPLE_NAME:
				return new SingleNameReference(value.toCharArray(), pos(source));
			case IMemberValuePair.K_BOOLEAN:
				return "true".equals(value) ? new TrueLiteral(sourceStart, sourceEnd) : new FalseLiteral(sourceStart, sourceEnd);
			case IMemberValuePair.K_INT:
				return makeIntLiteral(new char[]{'0'}, source);
			case IMemberValuePair.K_DOUBLE:
				return new DoubleLiteral(value.toCharArray(), sourceStart, sourceEnd);
		}			
		//TODO handle Array types
		return new StringLiteral(value.toCharArray(), sourceStart, sourceEnd, 0);
	}

	public static IType findTypeByFQName(String stereoTypeName) throws CoreException {
		IWorkspaceRoot workspaceRoot = null;
		try {
			workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		} catch (IllegalStateException ise) {
			//"workspace is closed"
			return null;
		}
		for (IProject project : workspaceRoot.getProjects()) {
			if (!project.isOpen()) {
				continue;
			}
			if (!project.hasNature(JavaCore.NATURE_ID)) {
				continue;
			}
			IJavaElement javaElement = (IJavaElement) project.getAdapter(IJavaElement.class);
			if (javaElement instanceof IJavaProject) {
				IType stereoType = ((IJavaProject)javaElement).findType(stereoTypeName);
				if (stereoType != null) {
					return stereoType;
				}
			}			
		}
		return null;
	}

	public static QualifiedTypeReference createQualifiedTypeReference(String qualifiedTypeName, ASTNode source) {
		char[][] elems = fromQualifiedName(qualifiedTypeName);
		return new QualifiedTypeReference(elems, poss(source, elems.length));
	}
}