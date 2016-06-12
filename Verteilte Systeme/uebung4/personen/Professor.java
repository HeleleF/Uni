package personen;

import java.io.Serializable;

/**
 * Professor <br>
 *
 * Ein serialisierbarer Professor <br>
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class Professor implements Serializable {

	private static final long serialVersionUID = 37131708312147369L;
	
	/** Eigenschaften fuer einen Professor */
	private String vorname, nachname, strasse, stadt, geburtsdatum, fachbereich;
	private int hausnummer, postleitzahl, persnr;
	
	public Professor() {
		super();
	}

	/**
	 * Konstruktor fuer Professor
	 * 
	 * @param vorname
	 *            der Vorname
	 * @param nachname
	 *            der Nachname
	 * @param strasse
	 *            die Strasse
	 * @param stadt
	 *            die Stadt
	 * @param geburtsdatum
	 *            das Geburtsdatum
	 * @param fachbereich
	 *            der Fachbereich
	 * @param hausnummer
	 *            die Hausnummer
	 * @param postleitzahl
	 *            die Postleitzahl
	 * @param persnr
	 *            die Personalnummer
	 */
	public Professor(String vorname, String nachname, String strasse, String stadt, String geburtsdatum,
			String fachbereich, int hausnummer, int postleitzahl, int persnr) {
		super();
		this.vorname = vorname;
		this.nachname = nachname;
		this.strasse = strasse;
		this.stadt = stadt;
		this.geburtsdatum = geburtsdatum;
		this.fachbereich = fachbereich;
		this.hausnummer = hausnummer;
		this.postleitzahl = postleitzahl;
		this.persnr = persnr;
	}
	
	@Override
	public String toString() {
		return "Professor [vorname=" + vorname + ", nachname=" + nachname + ", strasse=" + strasse + ", stadt=" + stadt
				+ ", geburtsdatum=" + geburtsdatum + ", fachbereich=" + fachbereich + ", hausnummer=" + hausnummer
				+ ", postleitzahl=" + postleitzahl + ", persnr=" + persnr + "]";
	}

	/**
	 * @return der Vorname
	 */
	public String getVorname() {
		return vorname;
	}

	/**
	 * @param vorname
	 *            der Vorname zum Setzen
	 */
	public void setVorname(String vorname) {
		this.vorname = vorname;
	}

	/**
	 * @return der Nachname
	 */
	public String getNachname() {
		return nachname;
	}

	/**
	 * @param nachname
	 *            der Nachname zum Setzen
	 */
	public void setNachname(String nachname) {
		this.nachname = nachname;
	}

	/**
	 * @return die Strasse
	 */
	public String getStrasse() {
		return strasse;
	}

	/**
	 * @param strasse
	 *            die Strasse zum Setzen
	 */
	public void setStrasse(String strasse) {
		this.strasse = strasse;
	}

	/**
	 * @return die Stadt
	 */
	public String getStadt() {
		return stadt;
	}

	/**
	 * @param stadt
	 *            die Stadt zum Setzen
	 */
	public void setStadt(String stadt) {
		this.stadt = stadt;
	}

	/**
	 * @return das Geburtsdatum
	 */
	public String getGeburtsdatum() {
		return geburtsdatum;
	}

	/**
	 * @param geburtsdatum
	 *            das Geburtsdatum zum Setzen
	 */
	public void setGeburtsdatum(String geburtsdatum) {
		this.geburtsdatum = geburtsdatum;
	}

	/**
	 * @return der Fachbereich
	 */
	public String getFachbereich() {
		return fachbereich;
	}

	/**
	 * @param fachbereich
	 *            der Fachbereich zum Setzen
	 */
	public void setFachbereich(String fachbereich) {
		this.fachbereich = fachbereich;
	}

	/**
	 * @return die Hausnummer
	 */
	public int getHausnummer() {
		return hausnummer;
	}

	/**
	 * @param hausnummer
	 *            die Hausnummer zum Setzen
	 */
	public void setHausnummer(int hausnummer) {
		this.hausnummer = hausnummer;
	}

	/**
	 * @return die Postleitzahl
	 */
	public int getPostleitzahl() {
		return postleitzahl;
	}

	/**
	 * @param postleitzahl
	 *            die Postleitzahl zum Setzen
	 */
	public void setPostleitzahl(int postleitzahl) {
		this.postleitzahl = postleitzahl;
	}

	/**
	 * @return die Personalnummer
	 */
	public int getPersnr() {
		return persnr;
	}

	/**
	 * @param persnr
	 *            die Personalnummer zum Setzen
	 */
	public void setPersnr(int persnr) {
		this.persnr = persnr;
	}

}
