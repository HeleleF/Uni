package de.htw_berlin.ai_bachelor.kbe.checklist8.validierung;

import javax.validation.Constraint;
import javax.validation.Payload;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Constraint(validatedBy = MyIntervalConstraint.class)
public @interface MyInterval {

	String message() default "{de.htw_berlin.ai_bachelor.kbe.checklist8.validierung.MyInterval}";
	
	Class<?>[] groups() default {};
	
	Class<? extends Payload>[] payload() default {};
}
