package client;

import java.net.*;

import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;

import org.w3c.dom.*;

import java.io.*;

/**
 * TCPClient <br>
 *
 * Ein TCP-Client <br>
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class TCPClient {
		
	/** Portnummer */
	final static int PORT = 9876;

	/**
	 * main Methode 
	 * 
	 * @param args Stringarray, ungenutzt
	 */
	public static void main(String[] args) {
	
		try {
			
			// zum Server verbinden
			System.out.println("Verbindung zum Server wird hergestellt...");
			Socket s = new Socket("localhost", PORT);
			System.out.println("Verbindung zum Server hergestellt!");
			
			// Ressourcen erstellen
			DataInputStream in = new DataInputStream(s.getInputStream());
			DataOutputStream out = new DataOutputStream(s.getOutputStream());			
			BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
			
			// fuer die Erstellung des XML-Dokuments
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			
			// fuer die Umwandlung XML-Dokument -> String
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			
			// zum Speichern der Serverantwort
			String antwort = "";
			
			do {
				// XML-Dokument anlegen und root-Element "personen" setzen
				Document xmldoc = docBuilder.newDocument();
				Element personen = xmldoc.createElement("personen");
				xmldoc.appendChild(personen);

				// auf Eingaben warten und XML-Struktur zusammenbauen
				while (true) {
					System.out.println(
							"\nAktion auswaehlen!\n\tp/P: Professor anlegen\n\ts/S: Student anlegen\n\ta/A: Eingabe abschliessen/abschicken");
					String str = br.readLine();
					if (str.equals("p") || str.equals("P")) {
						anlegenProf(xmldoc, personen, br);
					}
					if (str.equals("s") || str.equals("S")) {
						anlegenStud(xmldoc, personen, br);
					}
					if (str.equals("a") || str.equals("A")) {
						break;
					}
				}

				// XML-Struktur in String umwandeln
				DOMSource source = new DOMSource(xmldoc);
				StreamResult result = new StreamResult(new StringWriter());				
				transformer.transform(source, result);
				
				// XML-String an Server senden
				System.out.println("XML-Struktur erzeugt, wird gesendet!");
				out.writeUTF(result.getWriter().toString());
				System.out.println("Versandt, warten auf Serverantwort...");	
				
				// Serverantwort lesen und ausgeben
				antwort = in.readUTF();
				System.out.println("Antwort:"+antwort);
				
				// solange die Serverantwort nicht "Erfolg" ist, wiederhole...
			}	while (!(antwort.equals("Erfolg")));
			
			// beenden
			closeAll(br,in,out,s);
			
		} catch (UnknownHostException uhe) {	
			System.out.println("Fehlerhafter Host!: " + uhe.getMessage());
		} catch (ParserConfigurationException pce) {
			System.out.println("Fehler beim Parsen!: " + pce.getMessage());
		} catch (IOException ioe) {
			System.out.println("IO Fehler!: " + ioe.getMessage());
		} catch (TransformerException tfe) {
			System.out.println("Fehler beim Transformieren!: " + tfe.getMessage());
		}	
	}

	/**
	 * Hilfsmethode zum Erzeugen einer XML Struktur fuer einen Studenten <br>
	 * 
	 * fuegt nacheinander neue Elemente an das XML-Dokument an 
	 * und setzt die Benutzereingabe als deren Wert
	 *  
	 * @param doc das XML Dokument
	 * @param personen das root-Element der XML Struktur
	 * @param br BufferedReader fuer Eingabe
	 * @throws IOException
	 */
	private static void anlegenStud(Document doc, Element personen, BufferedReader br) throws IOException {	
		
//		Beispiel fuer eine XML-Struktur eines Studenten
		
//		<personen>	
//		  <student matnr="123456">
//		    <name>
//		      <vorname>Ivan</vorname>
//		      <nachname>Iwanowitsch</nachname>
//		    </name>
//		    <wohnort>
//		      <strasse>Ivanstrasse</strasse>
//		      <hausnummer>1</hausnummer>
//		      <postleitzahl>345</postleitzahl>
//		      <stadt>Ivanshausen</stadt>
//		    </wohnort>
//		    <studiengang>Angewandter Ivan Bachelor</studiengang>
//		    <fachsemester>1</fachsemester>
//		  </student>
//		</personen>
	
		Element student = doc.createElement("student");
		personen.appendChild(student);
		
		anlegenPerson(doc, student, br);

		System.out.println("Matrikelnummer eingeben:");	
		Attr attr = doc.createAttribute("matnr");
		attr.setValue(br.readLine());
		student.setAttributeNode(attr);

		System.out.println("Studiengang eingeben:");		
		Element studiengang = doc.createElement("studiengang");
		studiengang.appendChild(doc.createTextNode(br.readLine()));
		student.appendChild(studiengang);

		System.out.println("Fachsemester eingeben:");		
		Element fachsemester = doc.createElement("fachsemester");
		fachsemester.appendChild(doc.createTextNode(br.readLine()));
		student.appendChild(fachsemester);	
	}
	
	/**
	 * Hilfsmethode zum Erzeugen einer XML Struktur fuer einen Professor 
	 * 
	 * fuegt nacheinander neue Elemente an das XML-Dokument an 
	 * und setzt die Benutzereingabe als deren Wert
	 * 
	 * @param doc das XML Dokument
	 * @param personen das root-Element der XML Struktur
	 * @param br BufferedReader fuer Eingabe
	 * @throws IOException
	 */
	private static void anlegenProf(Document doc, Element personen, BufferedReader br) throws IOException {

//		Beispiel fuer eine XML-Struktur eines Professors
		
//		<personen>	
//		  <professor persnr="123456">
//		    <name>
//		      <vorname>Ivan</vorname>
//		      <nachname>Iwanowitsch</nachname>
//		    </name>
//		    <wohnort>
//		      <strasse>Ivanstrasse</strasse>
//		      <hausnummer>1</hausnummer>
//		      <postleitzahl>345</postleitzahl>
//		      <stadt>Ivanshausen</stadt>
//		    </wohnort>
//		    <geburtsdatum>2001-02-02</geburtsdatum>
//		    <fachbereich>Ivan fuer Fortgeschrittene</fachbereich>
//		  </professor>
//		</personen>
		
		Element professor = doc.createElement("professor");
		personen.appendChild(professor);
		
		anlegenPerson(doc, professor, br);
		
		System.out.println("Geburtsdatum eingeben:");
		Element geburtsdatum = doc.createElement("geburtsdatum");
		geburtsdatum.appendChild(doc.createTextNode(br.readLine()));
		professor.appendChild(geburtsdatum);
		
		System.out.println("Personalnummer eingeben:");	
		Attr attr = doc.createAttribute("persnr");
		attr.setValue(br.readLine());
		professor.setAttributeNode(attr);

		System.out.println("Fachbereich eingeben:");		
		Element fachbereich = doc.createElement("fachbereich");
		fachbereich.appendChild(doc.createTextNode(br.readLine()));
		professor.appendChild(fachbereich);	
	}
	
	/**
	 * Hilfsmethode zum Erzeugen einer XML Struktur fuer eine Person
	 * 
	 * fuegt nacheinander neue Elemente an das XML-Dokument an 
	 * und setzt die Benutzereingabe als deren Wert
	 * 
	 * @param doc das XML Dokument
	 * @param spezi spezifizierendes Element, ist "student" oder "professor"
	 * @param br BufferedReader fuer Eingabe
	 * @throws IOException
	 */
	private static void anlegenPerson(Document doc, Element spezi, BufferedReader br) throws IOException{
		
//		Beispiel fuer eine XML-Struktur einer Person
		
//		<personen>	
//		  <spezi>
//		    <name>
//		      <vorname>Ivan</vorname>
//		      <nachname>Iwanowitsch</nachname>
//		    </name>
//		    <wohnort>
//		      <strasse>Ivanstrasse</strasse>
//		      <hausnummer>1</hausnummer>
//		      <postleitzahl>345</postleitzahl>
//		      <stadt>Ivanshausen</stadt>
//		    </wohnort>
//		  </spezi>
//		</personen>
		
		Element name = doc.createElement("name");
		spezi.appendChild(name);
		
		System.out.println("Vorname eingeben:");		
		Element vorname = doc.createElement("vorname");
		vorname.appendChild(doc.createTextNode(br.readLine()));
		name.appendChild(vorname);

		System.out.println("Nachname eingeben:");		
		Element nachname = doc.createElement("nachname");
		nachname.appendChild(doc.createTextNode(br.readLine()));
		name.appendChild(nachname);
		
		Element wohnort = doc.createElement("wohnort");
		spezi.appendChild(wohnort);
		
		System.out.println("Strasse eingeben:");	
		Element strasse = doc.createElement("strasse");
		strasse.appendChild(doc.createTextNode(br.readLine()));
		wohnort.appendChild(strasse);

		System.out.println("Hausnummer eingeben:");		
		Element hausnummer = doc.createElement("hausnummer");
		hausnummer.appendChild(doc.createTextNode(br.readLine()));
		wohnort.appendChild(hausnummer);

		System.out.println("Postleitzahl eingeben:");		
		Element postleitzahl = doc.createElement("postleitzahl");
		postleitzahl.appendChild(doc.createTextNode(br.readLine()));
		wohnort.appendChild(postleitzahl);

		System.out.println("Stadt eingeben:");		
		Element stadt = doc.createElement("stadt");
		stadt.appendChild(doc.createTextNode(br.readLine()));
		wohnort.appendChild(stadt);
	}
	
	/**
	 * Hilfsmethode zum Schlieﬂen aller Ressourcen
	 * 
	 * @param br BufferedReader, der geschlossen werden soll
	 * @param in InputStream, der geschlossen werden soll
	 * @param out OutputStream, der geschlossen werden soll
	 * @param s Socket, das geschlossen werden soll
	 */
	private static void closeAll(BufferedReader br, InputStream in, OutputStream out, Socket s) {
		
		System.out.println("\nVersuche Ressourcen zu schliessen...");
		
		if (br != null) {
			try {
				br.close();
				System.out.println("\tReader geschlossen!");
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von br: " + ioe.getMessage());
			}
		}
		if (out != null) {
			try {
				out.close();
				System.out.println("\tOutputstream geschlossen!");
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von out: " + ioe.getMessage());
			}
		}
		if (in != null) {
			try {
				in.close();
				System.out.println("\tInputstream geschlossen!");
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von in: " + ioe.getMessage());
			}
		}
		if (s != null) {
			try {
				s.close();
				System.out.println("\tSocket geschlossen!");
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von s: " + ioe.getMessage());
			}
		}		
		System.out.println("\nClient beendet!");
		System.exit(0);
	}
}
