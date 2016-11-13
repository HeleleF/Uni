package de.htw_berlin.ai_bachelor.kbe.counter;

/**
 * Siehe PDF 5 Folie 29/30
 */
public class CounterFactory {
	
	public static final String CNT_SMP = "simple";
	public static final String CNT_DBL = "double";
	public static final String CNT_TRP = "triple";
	
	public static Counter getCounterInstance(String name) {
		
		// Name normalisieren, Leerzeichen weg und alles Klein
		String normal = name.trim().toLowerCase();

		if (CNT_SMP.equals(normal))
			return CounterSimple.getInstance();
		else if (CNT_DBL.equals(normal))
			return CounterDouble.getInstance();
		else if (CNT_TRP.equals(normal))
			return CounterTriple.getInstance();
		else
			throw new IllegalArgumentException(
					String.format("Counter vom Typ %s existiert nicht.", normal));

	}

}
