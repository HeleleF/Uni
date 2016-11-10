package de.htw_berlin.ai_bachelor.kbe.date;

public class Datum {
	
	private int day;
	private int month;
	private int year;
	
	public Datum (int day, int month, int year) {
		this.day = day;
		this.month = month;
		this.year = year;
	}
	
	public int getDay() {
		return day;
	}
	
	public void setDay(int day) {
		this.day = day;
	}
	
	public int getMonth() {
		return month;
	}
	
	public void setMonth(int month) {
		this.month = month;
	}
	
	public int getYear() {
		return year;
	}
	
	public void setYear(int year) {
		this.year = year;
	}
	
	@Override
	public String toString() {
		DatumFormatierer test = new DatumFormatiererCH();
		return test.format(this);
	}

}
