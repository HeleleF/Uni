package de.htw_berlin.ai_bachelor.kbe.counter;

public class CounterTriple implements Counter {
	
	private int counter = 1;
	
	private static CounterTriple ct = new CounterTriple();
	
	private CounterTriple() {		
	}
	
	public static CounterTriple getInstance() {
		return ct;
	}

	public void increment() {
		// TODO Auto-generated method stub
		this.counter *= 3;
	}

	public int getCount() {
		// TODO Auto-generated method stub
		return this.counter;
	}

}
