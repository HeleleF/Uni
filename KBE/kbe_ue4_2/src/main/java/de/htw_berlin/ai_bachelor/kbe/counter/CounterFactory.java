package de.htw_berlin.ai_bachelor.kbe.counter;

public class CounterFactory {
	
	public static final String CNT_SMP = "simple";
	public static final String CNT_DBL = "double";
	public static final String CNT_TRP = "triple";
	
	public static Counter getCounterInstance(String name) {
		
		String normalized = name.trim().toLowerCase();

		if (CNT_SMP.equals(normalized))
			return CounterSimple.getInstance();
		else if (CNT_DBL.equals(normalized))
			return CounterDouble.getInstance();
		else if (CNT_TRP.equals(normalized))
			return CounterTriple.getInstance();
		else
			throw new IllegalArgumentException(
					String.format("Counter vom Typ \"%s\" existiert nicht.", normalized));

	}

}
