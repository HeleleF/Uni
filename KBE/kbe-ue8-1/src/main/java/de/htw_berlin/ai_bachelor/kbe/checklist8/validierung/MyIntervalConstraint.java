package de.htw_berlin.ai_bachelor.kbe.checklist8.validierung;

import javax.el.ELContext;
import javax.el.ELResolver;

import javax.faces.context.FacesContext;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import de.htw_berlin.ai_bachelor.kbe.checklist8.model.Interval;

public class MyIntervalConstraint implements ConstraintValidator<MyInterval, Integer> {
	
	private Interval interval;

	@Override
	public void initialize(MyInterval meinInterval) {
		
		// EL-Ausdruck holen "intervalMB.interval"
		String intervalSrc = meinInterval.interval();
		
		// EL-Resolver beziehen mithilfe von FacesContext
		ELContext elContext = FacesContext.getCurrentInstance().getELContext();
		ELResolver resolver = elContext.getELResolver();
		
		// EL-Ausdruck auswerten
		Object obj = null;
		for (final String prop : intervalSrc.split("\\.")) {		
			obj = resolver.getValue(elContext, obj, prop);
		}

		// wenn ein Interval-Objekt erzeugt worden ist, zuweisen
		if (obj instanceof Interval) {
			interval = (Interval) obj;
		} else {
			throw new IllegalArgumentException("Kein gueltiges Interval-Objekt!");
		}		
	}
     
	@Override
	public boolean isValid(Integer prio, ConstraintValidatorContext constValCtx) {
		
		// siehe JSR 303; wenn null, dann True
         if (prio == null) 
           return true; 	 
		
		boolean drin = prio.compareTo(interval.getMin()) <= 0 && prio.compareTo(interval.getMax()) >= 0;
		
		return drin;
	}

}
