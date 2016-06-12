package server;

import java.net.*;

import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.*;

import org.xml.sax.SAXException;

import serialisierer.SerialisiererKlasse;

import java.nio.charset.StandardCharsets;

import java.io.*;

/** Connection <br>
*
* Thread fuer TCPServer <br>
* 
* @author Chris Rebbelin s0548921
* @version 1.2
*/
public class Connection extends Thread {
	
	/** InputStream Ressource*/
	DataInputStream in;
	/** OutputStream Ressource*/
	DataOutputStream out;
	/** Socket Ressource*/
	Socket clientSocket;
	
	/** Validator */
	Validator validator;
	
	/**
	 * Konstruktor fuer Connection
	 * 
	 * @param aClientSocket Socket des Clients
	 */
	public Connection(Socket aClientSocket) {
		try {
			System.out.println("Client angedockt!");
			
			// Ressourcen erstellen
			clientSocket = aClientSocket;
			in = new DataInputStream(clientSocket.getInputStream());
			out = new DataOutputStream(clientSocket.getOutputStream());
			
			// Schemadatei aus Projektverzeichnis laden 
			// (relativer Pfad; Verzeichnis, aus dem heraus das Programm gestartet wurde)
			// und Validatorobjekt erzeugen
			File schemaFile = new File("./personen.xsd");
			SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
			Schema schema = schemaFactory.newSchema(schemaFile);		
			validator = schema.newValidator();
			
			// run
			this.start();
		} catch (IOException ioe) {
			System.out.println("IO Fehler!: " + ioe.getMessage());
		} catch (SAXException saxe) {
			System.out.println("SAX Fehler!: " + saxe.getMessage());
		}	
	}
	
	@Override
	public void run() {
		
		// zum Speichern der Antwort
		String rueck = "";
		// Status
		boolean erfolg;
		
		do { 
			erfolg = true;
			try {
				
				// String von Client einlesen
				String data = in.readUTF();
				System.out.println("Datei von Client erhalten, validiere...\n");

				// String in XML-Dokument umwandeln und gegen das Schema validieren
				StreamSource source = new StreamSource(new ByteArrayInputStream(data.getBytes(StandardCharsets.UTF_8)));
				validator.validate(source);
				
				// an dieser Stelle muss XML valide sein; Serverantwort vorbereiten
				System.out.println("Valides XML!");	
				rueck = "Erfolg";
				
				SerialisiererKlasse.starteSerialisierung(data);
				
			} catch (EOFException eofe) {
				System.out.println("Unerwartetes Dateiende!: " + eofe.getMessage());
			} catch (IOException ioe) {
				System.out.println("IO Fehler beim Lesen!: " + ioe.getMessage());
			} catch (SAXException saxe) {
				
				// wenn XML-Dokument nicht validiert werden konnte, wird SAXException ausgeloest
				// Serverantwort vorbereiten und genaue Fehlermeldung anhaengen
				System.out.println("Fehlerhaftes XML! Sende Fehler zum Client...");		
				rueck = "Fehler!: " + saxe.getLocalizedMessage();
				
				// Validierung fehlgeschlagen, muss also wiederholt werden
				erfolg = false;
			}
			
			try {
				
				// Serverantwort an Client senden
				out.writeUTF(rueck);
			} catch (IOException ioe) {
				System.out.println("IO Fehler beim Schreiben!: " + ioe.getMessage());
			}
			
			// solange erfolg nicht TRUE ist, wiederhole...	
		} while (!erfolg);

		closeAll();	
	}

	/**
	 * Hilfsmethode zum Schlieﬂen aller Ressourcen
	 */
	private void closeAll() {
		if (out != null) {
			try {
				out.close();
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von out: " + ioe.getMessage());
			}
		}
		if (in != null) {
			try {
				in.close();
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von in: " + ioe.getMessage());
			}
		}
		if (clientSocket != null) {
			try {
				clientSocket.close();
			} catch (IOException ioe) {
				System.out.println("\tFehler beim Schlieﬂen von ClientSocket: " + ioe.getMessage());
			}
		}	
		System.out.println("Client abgedockt!");		
	}
}