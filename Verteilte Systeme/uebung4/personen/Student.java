package personen;

import java.io.Serializable;

/**
 * Student <br>
 *
 * Ein serialisierbarer Student <br>
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class Student implements Serializable {

	private static final long serialVersionUID = 6620406348297869377L;
	
	/** Eigenschaften fuer einen Student */
	private String vorname, nachname, strasse, stadt, studiengang;
	private int hausnummer, postleitzahl, matnr, fachsemester;
	
	public Student() {
		super();
	}
	
	/**
	 * Konstruktor fuer Student
	 * 
	 * @param vorname
	 *            der Vorname
	 * @param nachname
	 *            der Nachname
	 * @param strasse
	 *            die Strasse
	 * @param stadt
	 *            die Stadt
	 * @param studiengang
	 *            der Studiengang
	 * @param hausnummer
	 *            die Hausnummer
	 * @param postleitzahl
	 *            die Postleitzahl
	 * @param matnr
	 *            die Matrikelnummer
	 * @param fachsemester
	 *            das Fachsemester
	 */
	public Student(String vorname, String nachname, String strasse, String stadt, String studiengang, int hausnummer,
			int postleitzahl, int matnr, int fachsemester) {
		super();
		this.vorname = vorname;
		this.nachname = nachname;
		this.strasse = strasse;
		this.stadt = stadt;
		this.studiengang = studiengang;
		this.hausnummer = hausnummer;
		this.postleitzahl = postleitzahl;
		this.matnr = matnr;
		this.fachsemester = fachsemester;
	}

	@Override
	public String toString() {
		return "Student [vorname=" + vorname + ", nachname=" + nachname + ", strasse=" + strasse + ", stadt=" + stadt
				+ ", studiengang=" + studiengang + ", hausnummer=" + hausnummer + ", postleitzahl=" + postleitzahl
				+ ", matnr=" + matnr + ", fachsemester=" + fachsemester + "]";
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
	 * @return der Studiengang
	 */
	public String getStudiengang() {
		return studiengang;
	}

	/**
	 * @param studiengang
	 *            der Studiengang zum Setzen
	 */
	public void setStudiengang(String studiengang) {
		this.studiengang = studiengang;
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
	 * @return die Matrikelnummer
	 */
	public int getMatnr() {
		return matnr;
	}

	/**
	 * @param matnr
	 *            die Matrikelnummer zum Setzen
	 */
	public void setMatnr(int matnr) {
		this.matnr = matnr;
	}

	/**
	 * @return das Fachsemester
	 */
	public int getFachsemester() {
		return fachsemester;
	}

	/**
	 * @param fachsemester
	 *            das Fachsemester zum Setzen
	 */
	public void setFachsemester(int fachsemester) {
		this.fachsemester = fachsemester;
	}
}
