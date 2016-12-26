package de.htw_berlin.ai_bachelor.kbe.checklist8.model;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

public class Interval {

	private int _min;
	
	@Min(1) 
	@NotNull
	private int _max;

	public Interval(int min, int max) {
		this._min = min;
		this._max = max;
	}

	public int getMin() {
		return _min;
	}

	public void setMin(int min) {
		this._min = min;
	}

	public int getMax() {
		return _max;
	}

	public void setMax(int max) {
		this._max = max;
	}
}
