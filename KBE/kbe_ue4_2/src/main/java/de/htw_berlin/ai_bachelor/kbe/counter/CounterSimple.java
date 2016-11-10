package de.htw_berlin.ai_bachelor.kbe.counter;

public class CounterSimple implements Counter {
	
	private int counter = 0;
	
	private static CounterSimple cs = new CounterSimple();
	
	private CounterSimple() {		
	}
	
	public static CounterSimple getInstance() {
		return cs;
	}

	public void increment() {
		// TODO Auto-generated method stub
		this.counter++;		
	}

	public int getCount() {
		// TODO Auto-generated method stub
		return this.counter;
	}

}
