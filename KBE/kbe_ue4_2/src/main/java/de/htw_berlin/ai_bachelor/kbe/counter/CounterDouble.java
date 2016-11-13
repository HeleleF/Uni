package de.htw_berlin.ai_bachelor.kbe.counter;

/**
 * Siehe PDF 5 Folie 20
 */
public class CounterDouble implements Counter {
	
	private int counter = 1;
	
	private static CounterDouble cd = new CounterDouble();
	
	private CounterDouble() {		
	}
	
	public static CounterDouble getInstance() {
		return cd;
	}

	public void increment() {
		// TODO Auto-generated method stub
		this.counter *= 2;	
	}

	public int getCount() {
		// TODO Auto-generated method stub
		return this.counter;
	}

}
