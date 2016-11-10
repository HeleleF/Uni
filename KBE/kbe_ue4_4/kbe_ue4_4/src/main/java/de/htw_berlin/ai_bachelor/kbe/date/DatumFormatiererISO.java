package de.htw_berlin.ai_bachelor.kbe.date;

public class DatumFormatiererISO implements DatumFormatierer {

	public String format(Datum datum) {
		// TODO Auto-generated method stub
		return String.format("%d-%02d-%02d", datum.getYear(), datum.getMonth(), datum.getDay());
	}

}
