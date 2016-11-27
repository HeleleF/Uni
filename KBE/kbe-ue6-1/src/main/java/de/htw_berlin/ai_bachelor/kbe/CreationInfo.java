package de.htw_berlin.ai_bachelor.kbe; //siehe b)

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CreationInfo { //siehe d)

	String author() default "Ich der Chris";

	String description();

	String[] tags() default {};
	
	boolean baseclass() default true; //siehe e)
	
	boolean interfaces() default true; //siehe e)

}
