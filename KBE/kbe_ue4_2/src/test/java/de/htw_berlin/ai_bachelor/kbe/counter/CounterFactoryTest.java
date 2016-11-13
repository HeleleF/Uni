package de.htw_berlin.ai_bachelor.kbe.counter;

import static org.junit.Assert.*;
import org.junit.Test;

public class CounterFactoryTest {
	
	private Counter testcounter;
		
	@Test(expected = IllegalArgumentException.class)
	public void testFabrikgetInstanceExistiertNicht () {
		CounterFactory.getCounterInstance("lirumlarum");
	}
		
	@Test
	public void testFabrikgetSimpleInstance() {
		testcounter = CounterFactory.getCounterInstance("simple");
		
		assertEquals (testcounter, CounterFactory.getCounterInstance("simple")	
		);
	}
	
	@Test
	public void testFabrikgetDoubleInstance() {
		testcounter = CounterFactory.getCounterInstance("double");
		
		assertEquals (testcounter, CounterFactory.getCounterInstance("double")	
		);
	}
	
	@Test
	public void testFabrikgetTripleInstance() {
		testcounter = CounterFactory.getCounterInstance("triple");
		
		assertEquals (testcounter, CounterFactory.getCounterInstance("triple")	
		);
	}
}
