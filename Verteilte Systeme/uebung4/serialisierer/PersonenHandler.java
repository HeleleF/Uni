package serialisierer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import personen.Professor;
import personen.Student;

/**
 * PersonenHandler <br>
 *
 * Handler fuer SAXParser <br>
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class PersonenHandler extends DefaultHandler {

	/** Student- und Professorobjekt*/
	Student student;
	Professor professor;
	
	/** Schalter fuer Parser */
	boolean istStudent;
	boolean istProfessor;
	boolean istVorname;
	boolean istNachname;
	boolean istStrasse;
	boolean istStadt;
	boolean istHausnummer;
	boolean istPostleitzahl;
	boolean istGeburtsdatum ;
	boolean istFachbereich;
	boolean istFachsemester;
	boolean istStudiengang;

	/** Listen zum Speichern von Student- und Professorobjekten */
	public List<Student> studentenliste;
	public List<Professor> professorenliste;

	@Override
	public void startDocument() throws SAXException {

		// Methode wird zum Start der XML-Struktur aufgerufen
		
		System.out.println("Beginne Serialisierung...");

		// Listen initialisieren
		studentenliste = new ArrayList<Student>();
		professorenliste = new ArrayList<Professor>();
		
		// Schalter initialisieren
		istStudent = false;
		istProfessor = false;
		istVorname = false;
		istNachname = false;
		istStrasse = false;
		istStadt = false;
		istHausnummer = false;
		istPostleitzahl = false;
		istGeburtsdatum = false;
		istFachbereich = false;
		istFachsemester = false;
		istStudiengang = false;
	}

	@Override
	public void endDocument() throws SAXException {

		// Methode wird am Ende der XML-Struktur aufgerufen
		
		// alle erzeugten Objekte abspeichern
		abspeichern(studentenliste, "studenten");
		abspeichern(professorenliste, "professoren");

		System.out.println("Serialisierung abgeschlossen!");
	}

	/**
	 * Hilfsmethode zum Abspeichern aller Objekte einer Objektliste
	 * 
	 * @param liste eine Objektliste
	 * @param typ Objektbezeichner, entweder "studenten" oder "professoren"
	 */
	private void abspeichern(List<?> liste, String typ) {

		// nur wenn Liste ueberhaupt Objekte enthaelt...
		if (liste.size() > 0) {

			// Ordner erstellen
			erstelleOrdner(typ);

			// alle Objekte nacheinander serialisieren / speichern
			for (Object o : liste) {
				serialisiereObjekt(o, typ);
			}
			System.out.println(liste.size() + " " + typ + " erzeugt!");
		}
	}

	/**
	 * Hilfsmethode zum Erstellen der Ordner
	 * 
	 * @param ordnername Name des Ordners, entweder "studenten" oder "professoren"
	 */
	private void erstelleOrdner(String ordnername) {

		// neuer Ordner befindet sich im Projektverzeichnis 
		// (relativer Pfad; Verzeichnis, aus dem heraus das Programm gestartet wurde)
		File dir = new File("./" + ordnername);

		// wenn Ordner nicht existiert, versuche ihn zu erstellen...
		if (!dir.exists()) {
			if (dir.mkdir()) {
				System.out.println("Ordner /" + ordnername + " erstellt!");
			} else {
				System.out.println("Ordner /" + ordnername + " konnte nicht erstellt werden!");
			}
		} else {
			System.out.println("Ordner /" + ordnername + " bereits vorhanden!");
		}
	}

	/**
	 * Hilfsmethode zum Serialisieren eines Objektes
	 * 
	 * @param o das zu serialisierende Objekt
	 * @param typ Objektbezeichner, entweder "studenten" oder "professoren"
	 */
	private void serialisiereObjekt(Object o, String typ) {

		// benotigte Streams
		ObjectOutputStream oos = null;
		FileOutputStream fos = null;

		try {
			if (typ.equals("studenten")) {
				// Objekt ist vom Typ Student,
				// wird gespeichert unter /studenten/MATNR.ser
				fos = new FileOutputStream(typ + "/" + ((Student) o).getMatnr() + ".ser");
				
			} else {
				// Objekt ist vom Typ Professor
				// wird gespeichert unter /professoren/PERSNR.ser
				fos = new FileOutputStream(typ + "/" + ((Professor) o).getPersnr() + ".ser");
			}
			
			// schreiben
			oos = new ObjectOutputStream(fos);
			oos.writeObject(o);
			
		} catch (IOException ioe) {
			System.out.println("IO Fehler!: " + ioe.getMessage());
			
		} finally {
			// Streams wieder schlieﬂen
			if (oos != null)
				try {
					oos.close();
				} catch (IOException e) {
				}
			if (fos != null)
				try {
					fos.close();
				} catch (IOException e) {
				}
		}
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		//Methode wird immer aufgerufen, wenn neues XML Element beginnt
		
		// wenn ein "professor" Element gefunden wurde,
		// neues Professor Objekt erzeugen,
		// Personalnummer setzen und
		// Schalter auf TRUE
		if (qName.equals("professor")) {
			professor = new Professor();
			istProfessor = true;
			professor.setPersnr(Integer.parseInt(attributes.getValue(0)));
		}

		// wenn ein "student" Element gefunden wurde,
		// neues Student Objekt erzeugen,
		// Matrikelnummer setzen und
		// Schalter auf TRUE setzen
		if (qName.equals("student")) {
			student = new Student();
			istStudent = true;
			student.setMatnr(Integer.parseInt(attributes.getValue(0)));
		}

		// wenn Student Schalter auf TRUE steht...
		if (istStudent) {

			// wenn Element gefunden wurde, fuer das ein Schalter existiert,
			// diesen Schalter auf TRUE setzen
			switch (qName) {
			case "vorname":
				istVorname = true;
				break;
			case "nachname":
				istNachname = true;
				break;
			case "strasse":
				istStrasse = true;
				break;
			case "hausnummer":
				istHausnummer = true;
				break;
			case "postleitzahl":
				istPostleitzahl = true;
				break;
			case "stadt":
				istStadt = true;
				break;
			case "studiengang":
				istStudiengang = true;
				break;
			case "fachsemester":
				istFachsemester = true;
				break;
			default:
				break;
			}
		}

		// wenn Professor Schalter auf TRUE steht...
		if (istProfessor) {

			// wenn Element gefunden wurde, fuer das ein Schalter existiert,
			// diesen Schalter auf TRUE setzen
			switch (qName) {
			case "vorname":
				istVorname = true;
				break;
			case "nachname":
				istNachname = true;
				break;
			case "strasse":
				istStrasse = true;
				break;
			case "hausnummer":
				istHausnummer = true;
				break;
			case "postleitzahl":
				istPostleitzahl = true;
				break;
			case "stadt":
				istStadt = true;
				break;
			case "geburtsdatum":
				istGeburtsdatum = true;
				break;
			case "fachbereich":
				istFachbereich = true;
				break;
			default:
				break;
			}
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		//Methode wird immer aufgerufen, wenn neues XML Element endet
		
		// wenn ein "professor" Element beendet wurde,
		// Professor Objekt der Liste hinzufuegen und
		// Schalter wieder auf FALSE setzen
		if (qName.equals("professor")) {
			professorenliste.add(professor);
			istProfessor = false;
		}

		// wenn ein "student" Element beendet wurde,
		// Student Objekt der Liste hinzufuegen und
		// Schalter wieder auf FALSE setzen
		if (qName.equals("student")) {
			studentenliste.add(student);
			istStudent = false;
		}

		// wenn Student Schalter auf TRUE steht...
		if (istStudent) {
			
			// wenn Element beendet wurde, fuer das ein Schalter existiert,
			// diesen Schalter wieder auf FALSE setzen
			switch (qName) {
			case "vorname":
				istVorname = false;
				break;
			case "nachname":
				istNachname = false;
				break;
			case "strasse":
				istStrasse = false;
				break;
			case "hausnummer":
				istHausnummer = false;
				break;
			case "postleitzahl":
				istPostleitzahl = false;
				break;
			case "stadt":
				istStadt = false;
				break;
			case "studiengang":
				istStudiengang = false;
				break;
			case "fachsemester":
				istFachsemester = false;
				break;
			default:
				break;
			}
		}

		// wenn Professor Schalter auf TRUE steht...
		if (istProfessor) {
			
			// wenn Element beendet wurde, fuer das ein Schalter existiert,
			// diesen Schalter wieder auf FALSE setzen
			switch (qName) {
			case "vorname":
				istVorname = false;
				break;
			case "nachname":
				istNachname = false;
				break;
			case "strasse":
				istStrasse = false;
				break;
			case "hausnummer":
				istHausnummer = false;
				break;
			case "postleitzahl":
				istPostleitzahl = false;
				break;
			case "stadt":
				istStadt = false;
				break;
			case "geburtsdatum":
				istGeburtsdatum = false;
				break;
			case "fachbereich":
				istFachbereich = false;
				break;
			default:
				break;
			}
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {

		// Methode verarbeitet Inhalt von XML Elementen
		
		// Leerzeichen entfernen und leere Inhalt ignorieren
		String value = new String(ch, start, length).trim();
		if (value.length() == 0)
			return;

		// wenn Student Schalter auf TRUE steht...
		if (istStudent) {
			
			// und ein Element Schalter auf TRUE steht,
			// Wert zuweisen
			if (istVorname)
				student.setVorname(value);

			if (istNachname)
				student.setNachname(value);

			if (istStrasse)
				student.setStrasse(value);

			if (istHausnummer)
				student.setHausnummer(Integer.parseInt(value));

			if (istPostleitzahl)
				student.setPostleitzahl(Integer.parseInt(value));

			if (istStadt)
				student.setStadt(value);

			if (istStudiengang)
				student.setStudiengang(value);

			if (istFachsemester)
				student.setFachsemester(Integer.parseInt(value));
		}

		// wenn Professor Schalter auf TRUE steht...
		if (istProfessor) {
			
			// und ein Element Schalter auf TRUE steht,
			// Wert zuweisen
			if (istVorname)
				professor.setVorname(value);

			if (istNachname)
				professor.setNachname(value);

			if (istStrasse)
				professor.setStrasse(value);

			if (istHausnummer)
				professor.setHausnummer(Integer.parseInt(value));

			if (istPostleitzahl)
				professor.setPostleitzahl(Integer.parseInt(value));

			if (istStadt)
				professor.setStadt(value);

			if (istGeburtsdatum)
				professor.setGeburtsdatum(value);

			if (istFachbereich)
				professor.setFachbereich(value);
		}
	}
}
