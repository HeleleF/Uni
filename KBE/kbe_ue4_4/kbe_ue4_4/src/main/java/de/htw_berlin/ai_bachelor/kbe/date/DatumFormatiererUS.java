package de.htw_berlin.ai_bachelor.kbe.date;

public class DatumFormatiererUS implements DatumFormatierer {

	public String format(Datum datum) {
		// TODO Auto-generated method stub
		return String.format("%02d/%02d/%d", datum.getMonth(), datum.getDay(), datum.getYear());
	}

}
