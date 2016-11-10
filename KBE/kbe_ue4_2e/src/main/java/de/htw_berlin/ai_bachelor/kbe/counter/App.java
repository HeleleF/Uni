package de.htw_berlin.ai_bachelor.kbe.counter;

/**
 * Hello world!
 *
 */
public class App {
	public static void main(String[] args) {
		Counter cnt = CounterFactory.getCounterInstance("simple");
		cnt.increment();
		cnt.increment();
		System.out.println("Counterwert: " + cnt.getCount());
	}
}
