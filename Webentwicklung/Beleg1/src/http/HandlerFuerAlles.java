package http;

import java.io.*;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import com.sun.net.httpserver.*;

/**
 * HandlerFuerAlles <br>
 * 
 * Handler fuer den Webserver
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class HandlerFuerAlles implements HttpHandler {

	// Konstanten

	/** UTF-8 Charset als String */
	final static String CHARSET = StandardCharsets.UTF_8.name();

	/** absoluter Nullpunkt in °C */
	final static double NULLPKT = -273.15;

	// Statische Variablen

	/** Variable zum Speichern des vorherigen Druckwertes */
	static double pAlt = 1;

	/** Variable zum Speichern des vorherigen Temperaturwertes */
	static double tAlt = 20;

	/** Variable zum Speichern des vorherigen Dichtewertes */
	static double dAlt = 0.9982;

	/** Variable zum Speichern des vorherigen Seitenquelltextes */
	static String htmlAlt = initStr();

	/**
	 * Handlermethode <br>
	 * 
	 * Bearbeitet einen HTTP Request und erstellt Antwort
	 * 
	 * @param t
	 *            HttpExchange, der den Request enthaelt
	 * @throws IOException
	 */
	@Override
	public void handle(HttpExchange t) throws IOException {

		// Kontextpfad speichern
		String content = t.getHttpContext().getPath();

//		System.out.println(System.getProperty("user.dir"));

		// Behandle alles standardmaessig als html,
		String contentType = "text/html";
		
		// wenn Kontextpfad "css" enthaelt, wechsle zu css
		if (content.contains("css")) {
			contentType = "text/css";
			// bei Bildern zu svg
		} else if (content.contains("image")) {
			contentType = "image/svg+xml";
		}

		// GET Request
		if (t.getRequestMethod().equals("GET")) {

			// benoetigte Datei aus Projektverzeichnis lesen
			File datei = new File("." + content);
			byte[] bytearray = new byte[(int) datei.length()];
			FileInputStream fis = new FileInputStream(datei);
			BufferedInputStream bis = new BufferedInputStream(fis);
			bis.read(bytearray, 0, bytearray.length);
			bis.close();

			// Antwort senden
			antworten(t, contentType, datei.length(), bytearray);

			// POST Request
		} else if (t.getRequestMethod().equals("POST")) {

			// Map zum Speichern von (Name,Value)-Paaren
			Map<String, Object> parameters = new HashMap<String, Object>();
			
			// Request-Body erhalten und parsen
			InputStreamReader isr = new InputStreamReader(t.getRequestBody(), CHARSET);
			BufferedReader br = new BufferedReader(isr);
			parse(br.readLine(), parameters);

			// String zum Speichern der Antwort
			String response = "";

			// wenn der "Calculate" Button gedrueckt wurde
			if (parameters.containsKey("calcer")) {
				
				// neuen Temperaturwert aus Map holen und als String speichern
				String tNeuStr = (String) parameters.get("opTemp");
				// neuen Druckwert aus Map holen und als String speichern
				String pNeuStr = (String) parameters.get("opPres");

				//Antwort erstellen
				response += calcAndReplace(tNeuStr, pNeuStr);

				if ("<h2>".equals(response)) {
					response += "Eingabefehler!</h2>";
				}
				// "Reset" Button gedrueckt
			} else {
				response += "<h2>Nicht implementiert!</h2>";
			}
			
			int offset = 0;
			byte[] antArr = response.toString().getBytes(CHARSET);
			
			// (um CONTENT_LENGTH_MISSMATCH Fehler zu vermeiden)
			// Laenge des Bytearrays ist groesser als Laenge des Strings wegen BOM-Zeichen
			// damit kein Fehler auftritt, muss die Contentlaenge um die Differenz der Laengen erhoeht werden
			if (antArr.length != response.length()) {
				offset = antArr.length - response.length();
			}
			// Antwort senden
			antworten(t, contentType, response.length() + offset, antArr);
		}
	}

	/**
	 * Methode zum Initialisieren von {@code htmlAlt}
	 * 
	 * @return (modifizierter) Quelltext der Datei index.html als String
	 */
	public static String initStr() {
		
//		System.out.println(System.getProperty("user.dir"));

		try {
			// Versucht, index.html aus dem Projektverzeichnis zu lesen
			File datei = new File("./index.html");
			FileInputStream fis = new FileInputStream(datei);
			InputStreamReader reader = new InputStreamReader(fis, CHARSET);
			BufferedReader in = new BufferedReader(reader);
			
			// Inhalt der .html-Datei in String speichern
			String str;
			String s = "";
			while ((str = in.readLine()) != null) {
				//CRLF als End-of-Line-Zeichen
				s += str + "\r\n";
			}
			in.close();

			//fuer Doubleformat; ".0" anhaengen
			String u = s.replace("name=\"opTemp\" value=\"20\"", "name=\"opTemp\" value=\"20.0\"");
			String v = u.replace("name=\"opPres\" value=\"1\"", "name=\"opPres\" value=\"1.0\"");

			// gibt Quelltext als String zurueck; 
			return v;
			
			//bei Fehlern null-String zurueckgeben
		} catch (IOException | NullPointerException npe) {
			return null;
		}
	}

	/**
	 * Hilfsmethode zum Erstellen und Senden der Antwort
	 * 
	 * @param t
	 *            HttpExchange, der den Request enthaelt
	 * @param contentType
	 *            String, der contenttyp enthaelt
	 * @param laenge
	 *            Laenge des Response Body
	 * @param array
	 *            Bytearray, das zum Output geschrieben wird
	 * @throws IOException
	 */
	private void antworten(HttpExchange t, String contentType, long laenge, byte[] array) throws IOException {
		
		// Header schreiben
		Headers responseHeaders = t.getResponseHeaders();
		responseHeaders.set("Content-Type", contentType + ";charset=" + CHARSET);
		t.sendResponseHeaders(200, laenge);
		
		// Antwort schreiben
		OutputStream os = t.getResponseBody();
		os.write(array, 0, array.length);
		os.close();
	}

	/**
	 * Hilfsmethode zum Berechnen der Dichte <br>
	 * und zum Ersetzen der alten durch die neuen Werte
	 * 
	 * @param tNeuStr
	 *            eingegebener Temperaturwert als String
	 * @param pNeuStr
	 *            eingegebener Druckwert als String
	 * @return (modifizierter) Quelltext der Datei index.html als String
	 * 
	 */
	private String calcAndReplace(String tNeuStr, String pNeuStr) {
		try {
			// Versucht, Stringwerte als Double zu parsen
			double tNeu = Double.parseDouble(tNeuStr);
			double pNeu = Double.parseDouble(pNeuStr);

			// Temperatur und Druck haben physikalische Minima
			if (tNeu <= NULLPKT || pNeu <= 0) {
				return "<h2>";
			} else {

				// Formel nach Aufgabenstellung
				double dNeu = (-NULLPKT + tNeu) / (-NULLPKT + tAlt) * (pAlt / pNeu) * dAlt;

				// alte Werte in Quelltext durch neue Werte ersetzen
				String s2 = htmlAlt.replace("name=\"densi\" value=\"" + dAlt + "\"",
											"name=\"densi\" value=\"" + dNeu + "\"");
				String s3 = s2.replace("name=\"opTemp\" value=\"" + tAlt + "\"",
									   "name=\"opTemp\" value=\"" + tNeu + "\"");
				String htmlNeu = s3.replace("name=\"opPres\" value=\"" + pAlt + "\"",
											"name=\"opPres\" value=\"" + pNeu + "\"");

				// nur wenn neuer Dichtewert sinnvoll ist,
				// werden neue Werte als alte Werte gespeichert:
				// 0 ist nicht sinnvoll, da sich bei allen zukuenftigen Berechnungen 0 ergibt
				// "NaN", "+Infinity" und "-Infinity" sind nicht sinnvoll fuer Berechnungen
				if (dNeu != 0 && Double.isFinite(dNeu)) {
					tAlt = tNeu;
					pAlt = pNeu;
					dAlt = dNeu;
					htmlAlt = htmlNeu;
				}
				// modifizierten Quelltext zurueckgeben
				return htmlNeu;
			}
			// Parsen fehlgeschlagen oder replace-Methode fehlgeschlagen
		} catch (NumberFormatException | NullPointerException ne) {
			return "<h2>";
		}
	}

	/**
	 * Hilfsmethode zum Parsen des POST-Requests
	 * 
	 * @param query
	 *            POST-Request Body
	 * @param parameters
	 *            Map zum Speichern von Name/Value-Zugehoerigkeiten
	 * @throws UnsupportedEncodingException
	 */
	private void parse(String query, Map<String, Object> parameters) throws UnsupportedEncodingException {

		// parsen macht nur Sinn, wenn der Body NICHT null (leer) ist
		if (query != null) {
			// Body aufteilen in "Name=Value"-Teile
			String pairs[] = query.split("[&]");

			// fuer alle diese Teile,
			for (String pair : pairs) {

				// aufteilen in "Name"- und "Value"-Teile
				String param[] = pair.split("[=]");

				String key = null;
				String value = null;

				// Name decodieren
				if (param.length > 0) {
					key = URLDecoder.decode(param[0], CHARSET);
				}

				// Value decodieren
				if (param.length > 1) {
					value = URLDecoder.decode(param[1], CHARSET);
				}
				// Paar (Name,Value) zur Map hinzufuegen
				parameters.put(key, value);
			}
		}
	}
}