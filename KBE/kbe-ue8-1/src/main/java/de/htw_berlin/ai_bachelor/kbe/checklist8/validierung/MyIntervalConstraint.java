package de.htw_berlin.ai_bachelor.kbe.checklist8.validierung;

import javax.faces.context.FacesContext;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import javax.validation.constraints.NotNull;

import de.htw_berlin.ai_bachelor.kbe.checklist8.model.Interval;
import de.htw_berlin.ai_bachelor.kbe.checklist8.mb.IntervalMB;

public class MyIntervalConstraint implements ConstraintValidator<MyInterval, Integer> {

	@Override
	public void initialize(@NotNull final MyInterval meinInterval) {
	}

	@Override
	public boolean isValid(final Integer value, @NotNull final ConstraintValidatorContext context) {
		if (value == null)
			return true;
		else {
			final FacesContext fcontext = FacesContext.getCurrentInstance();
			final Interval interval = ((IntervalMB) fcontext.getApplication().
					evaluateExpressionGet(fcontext, "#{intervalMB}", IntervalMB.class)).
					getInterval();

			final int lowerLimit = interval.getMin();
			final int upperLimit = interval.getMax();
			final boolean isValid = (value >= lowerLimit && value <= upperLimit);

			if (!isValid) {
				context.disableDefaultConstraintViolation();
				final String ERROR_MESSAGE = String.format("Der Wert %d liegt nicht im erlaubten Bereich: [%d - %d]!",
						value, lowerLimit, upperLimit);
				context.buildConstraintViolationWithTemplate(ERROR_MESSAGE).addConstraintViolation();
			}

			return isValid;
		}
	}

}
