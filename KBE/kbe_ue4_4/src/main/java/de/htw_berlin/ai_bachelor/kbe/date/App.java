package de.htw_berlin.ai_bachelor.kbe.date;

/**
 * Hello world!
 *
 */
public class App {
	public static void main(String[] args) {
		
		Datum datum = new Datum(18,11,2001); // Datum selber weiß nichts von Formatierung
				
		DatumFormatierer test = new DatumFormatiererUS(); // Standardformat
		System.out.println("US: " + test.format(datum));
		
		test = new DatumFormatiererISO();	// Format wechseln
		System.out.println("ISO: " + test.format(datum));
	}
}
