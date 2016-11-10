package de.htw_berlin.ai_bachelor.kbe.counter;

import static org.junit.Assert.*;
import org.junit.Test;

public class CounterFactoryTest {
	@Test
	public void getInstanceSimple() {
		assertEquals(
			CounterSimple.class,
			CounterFactory.getCounterInstance("simple").getClass()
		);
	}
	
	@Test
	public void getInstanceSimpleFett() {
		assertEquals(
			CounterSimple.class,
			CounterFactory.getCounterInstance("SIMPLE").getClass()
		);
	}
	
	@Test
	public void getInstanceSimpleDif() {
		assertEquals(
			CounterSimple.class,
			CounterFactory.getCounterInstance(" SiMpLe ").getClass()
		);
	}
	
	@Test
	public void getInstanceDouble() {
		assertEquals(
			CounterDouble.class,
			CounterFactory.getCounterInstance("double").getClass()
		);
	}
	
	@Test
	public void getInstanceTriple() {
		assertEquals(
			CounterTriple.class,
			CounterFactory.getCounterInstance("triple").getClass()
		);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void getInstanceIllegalArgument () {
		CounterFactory.getCounterInstance("lirumlarum");
	}
	
	@Test
	public void getInstanceBadFormattedArgument() {
		assertEquals(
			CounterSimple.class,
			CounterFactory.getCounterInstance("   simple   ").getClass()
		);
	}
	
	@Test
	public void getInstanceSimpleExisting() {
		Counter counter = CounterFactory.getCounterInstance("simple");
		
		assertEquals (
			counter,
			CounterFactory.getCounterInstance("simple")	
		);
	}
	
	@Test
	public void getInstanceDoubleExisting() {
		Counter counter = CounterFactory.getCounterInstance("double");
		
		assertEquals (
			counter,
			CounterFactory.getCounterInstance("double")	
		);
	}
	
	@Test
	public void getInstanceTripleExisting() {
		Counter counter = CounterFactory.getCounterInstance("triple");
		
		assertEquals (
			counter,
			CounterFactory.getCounterInstance("triple")	
		);
	}
	
	@Test
	public void getInstanceNotExisting() {
		Counter counter = CounterFactory.getCounterInstance("simple");
		
		assertNotEquals (
			counter,
			CounterFactory.getCounterInstance("double")	
		);
	}
}
