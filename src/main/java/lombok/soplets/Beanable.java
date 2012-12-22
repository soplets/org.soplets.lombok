package lombok.soplets;
 
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
 
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Beanable {
	
	public Class javaType() default String.class;
	public int length() default 0;
}