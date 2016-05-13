package uebung3; //moeglicherweise auskommentieren, weil ImageJ damit nicht klarkommt

import ij.*;
import ij.gui.*;
import java.awt.*;
import java.util.Vector;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/**
 * Kontur <br>
 *
 * Klasse zum Testen von Konturverfolgung <br>
 * 
 * TODO: Bug fixen mit den 1-Pixel-großen Polygonen 
 *
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class Kontur implements PlugInFilter {

	private Vector<Polygon> polyvec;

	/** Initialisierung in ImageJ */
	public int setup(String arg, ImagePlus imp) {
		if (arg.equals("about")) {
			showAbout();
			return DONE;
		}
		// Zugelassen nur für 8-Bit Graubilder
		return DOES_8G + NO_CHANGES;
	}

	/** About Message zu diesem Plug-In. */
	void showAbout() {
		IJ.showMessage("Graubildtest", "Testprogramm");
	}

	/**
	 * Ausführende Funktion
	 * 
	 * @param ip
	 *            Image Processor. Klasse in ImageJ, beinhaltet das Bild und
	 *            zugehörige Metadaten.
	 */
	public void run(ImageProcessor ip) {

		// Bildlaenge, -hoehe und Region of Interest speichern
		int w = ip.getWidth();
		int h = ip.getHeight();
		Rectangle roi = ip.getRoi();

		// neues Bild erzeugen und mit schwarzen Pixeln fuellen
		ImagePlus corrected = NewImage.createByteImage("Konturenbild", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor cor_ip = corrected.getProcessor();

		// Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[]) ip.getPixels();
		// Pixelarray des neuen Bildes
		byte[] pixels = (byte[]) cor_ip.getPixels();

		/***********
		 * An dieser Stelle kann an den einzelnen Pixeln gearbeitet werden.
		 *********/

		konturen(pixelsin, pixels, roi, w, h);

		/***************** Ende **********************************************************/

		corrected.show();
		corrected.updateAndDraw();
	}

	/**
	 * Funktion zum Suchen von Startpunkten fuer die Konturverfolgung
	 * 
	 * @param pixelsin
	 *            Pixel-Array des Eingabebildes
	 * @param pixels
	 *            Pixelarray des neuen Bildes
	 * @param roi
	 *            Region of Interest
	 * @param w
	 *            Bildlaenge
	 * @param h
	 *            Bildhoehe
	 */
	private void konturen(byte[] pixelsin, byte[] pixels, Rectangle roi, int w, int h) {

		// Vektor zum Speichern aller Konturen
		polyvec = new Vector<Polygon>();

		// ueber alle Bildpixel iterieren
		for (int i = roi.y + 1; i < roi.y + roi.height - 1; i++) {
			for (int j = roi.x + 1; j < roi.x + roi.width - 1; j++) {

				// Wert an der Stelle [i,j] herausholen und konvertieren
				int kandidat = pixelsin[i * w + j];
				kandidat = (kandidat & 0x0000ff);

				// wenn Pixel SCHWARZ ist, ist er Objektpixel -> neue Kontur
				// beginnt
				if (kandidat == 0) {

					// wenn Pixel an der Stelle [i,j] NICHT zu einer Kontur
					// gehoert,
					// ist ein Kandidat fuer eine neue Kontur gefunden
					if (!schonVorhanden(i, j)) {
						erzeugeKontur(pixelsin, pixels, i, j, w);
					}
				}
			}
		}
		// ImageJ Debugger; Konturen zaehlen
		IJ.log("Anzahl: " + polyvec.size());
	}

	/**
	 * Funktion zum Erzeugen eines Konturpolygons Algorithmus aus Uebungsblatt 3
	 * 
	 * @param pixelsin
	 *            Pixel-Array des Eingabebildes
	 * @param pixels
	 *            Pixelarray des neuen Bildes
	 * @param x
	 *            X-Koordinate des Konturstartpunktes
	 * @param y
	 *            Y-Koordinate des Konturstartpunktes
	 * @param w
	 *            Bildlaenge
	 */
	private void erzeugeKontur(byte[] pixelsin, byte[] pixels, int x, int y, int w) {

		// neues Konturpolygon
		Polygon kontur = new Polygon();

		// Koordinaten fuer "sich bewegenden" Konturpixel
		int xNeu = x;
		int yNeu = y;

		// speichert die Ausrichtung als Integer
		int richtung = 0;

		do { // solange Neuer Punkt nicht Startpunkt ist, füge Punkte an die
				// Kontur an

			// 0 ist OBEN, 1 ist LINKS, 2 ist UNTEN, 3 ist RECHTS
			// Daraus folgt:
			// Nach Links abbiegen => richtung - 1
			// Nach Rechts abbiegen => richtung + 1
			// 4 entspricht 0 (im Uhrzeigersinn kommt nach RECHTS wieder OBEN)
			// -1 entspricht 3 (gegen Uhrzeigersinn kommt nach OBEN wieder
			// RECHTS
			//
			switch (richtung) {
			case 4:
				richtung = 0;
			case 0:
				xNeu -= 1;
				break;
			case 1:
				yNeu += 1;
				break;
			case 2:
				xNeu += 1;
				break;
			case -1:
				richtung = 3;
			case 3:
				yNeu -= 1;
				break;
			}

			// Wert herausholen und konvertieren
			int konturpixel = pixelsin[xNeu * w + yNeu];
			konturpixel = (konturpixel & 0x0000ff);

			// wenn Objektpixel
			if (konturpixel == 0) {

				// Kontur in neuem Bild weiß malen
				pixels[xNeu * w + yNeu] = (byte) 255;

				kontur.addPoint(xNeu, yNeu);
				// IJ.log("x,y koordinaten: " + xNeu + "," + yNeu);

				// nach links gehen
				richtung -= 1;

				// wenn Hintergrundpixel
			} else {

				// nach rechts gehen
				richtung += 1;
			}

		} while (xNeu != x || yNeu != y);

		// Konturpolygon zum Vektor hinzufügen
		this.polyvec.addElement(kontur);

		// ImageJ Debugger; Laenge der Kontur
		IJ.log("Kontur: " + kontur.npoints);
	}

	/**
	 * Ueberprueft, ob Punkt schon Teil einer existierenden Kontur ist
	 * 
	 * @param x
	 *            X-Koordinate
	 * @param y
	 *            Y-Koordinate
	 * @return TRUE, wenn Punkt Teil einer vorhandenen Kontur ist, sonst FALSE
	 */
	public boolean schonVorhanden(int x, int y) {
		// sucht Punkt INNERHALB des Konturpolygons
		for (int i = 0; i < this.polyvec.size(); i++) {
			Polygon poly = this.polyvec.elementAt(i);
			if (poly.contains(x, y))
				return true;

			// sucht Punkt AUF der Konturlinie selbst
			for (int j = 0; j < poly.npoints; j++) {
				if (x == poly.xpoints[j] && y == poly.ypoints[j])
					return true;
			}
		}
		return false;
	}
}
