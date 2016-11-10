package de.htw_berlin.ai_bachelor.kbe.date;

/**
 * Hello world!
 *
 */
public class App {
	public static void main(String[] args) {
		
		Datum datum = new Datum(18,11,2001);
		
		System.out.println("STANDARD: " + datum);
		
		DatumFormatierer test = new DatumFormatiererUS();		
		System.out.println("US: " + test.format(datum));
		
		test = new DatumFormatiererISO();		
		System.out.println("ISO: " + test.format(datum));
	}
}
