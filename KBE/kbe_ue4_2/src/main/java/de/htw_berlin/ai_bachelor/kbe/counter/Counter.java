package de.htw_berlin.ai_bachelor.kbe.counter;

//TODO Counter Interface erstellen

/**
 * Interface für einen Counter
 */
public interface Counter {
	
	/**
	 * Erhoehe Counter um einen festgelegten Wert
	 */
	public void increment();
	
	/**
	 * @return der momentane Wert des Counters
	 */
	public int getCount();
}
