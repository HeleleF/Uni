import ij.*;
import ij.gui.*;
import java.awt.*;
import java.util.Vector;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;
import java.util.List;
import java.util.ArrayList;

public class Belegaufgabe implements PlugInFilter {
	
	/** Initialisierung in ImageJ */
	private final int COLOR_R = 0;
	private final int COLOR_G = 1;
	private final int COLOR_B = 2;
	
	// Grenzen fuer die "Erkennung" der "perfekten" Kreise
	final static int RADIUS_GRENZE = 30;
	final static int ABWEICHUNG_GRENZE = 3;
	
	public int setup(String arg, ImagePlus imp) {
		if (arg.equals("about")) {
			showAbout();
			return DONE;
		}
		// Zugelassen fuer RGB Farbbilder
		return DOES_RGB + NO_CHANGES;
	}

	/** About Message zu diesem Plug-In. */
	void showAbout() {
		IJ.showMessage("Graubildtest", "Testprogramm");
	}

	/**
	 * Ausführende Funktion
	 * 
	 * @param ip
	 *            Image Processor. Klasse in ImageJ, beinhaltet das Bild und zugehörige Metadaten.          
	 */
	public void run(ImageProcessor ip) {

		// get width, height and the region of interest
		int w = ip.getWidth();
		int h = ip.getHeight();
		Rectangle roi = ip.getRoi();

		// create a new image with the same size and copy the pixels of the original image
		ImagePlus corrected = NewImage.createByteImage("corrected image", w, h, 1, NewImage.FILL_WHITE);
		ImageProcessor cor_ip = corrected.getProcessor();
		// cor_ip.copyBits(ip,0,0,Blitter.COPY);
		// Pixel-Array des Eingabebildes
		int[] pixelsin = (int[]) ip.getPixels();
		int[] pixels = pixelsin.clone();
		// Pixelarray des neuen Bildes
		byte[] pixelnew = (byte[]) cor_ip.getPixels();

		/************ An dieser Stelle kann an den einzelnen Pixeln gearbeitet werden.*********/
		// DER PLAN
		// Vorverarbeitung: Glaettung und Histogrammegalisierung
		// Konturverfolgung
		// Berechnen der Radien und Kreise
		// Ausgeben
		
		smooth(pixelsin, pixels, w, h);
		
		// histogramm egalisierung machen um auch die nicht sichtbaren Stanzen zu erkennen

		// Initialwerte fuer den Schwellenwert
		// an den jeweiligen Eckpunkten
		int[] tmp = new int[4];

		tmp[0] = pixels[0];
		tmp[1] = pixels[w - 1];
		tmp[2] = pixels[(h - 1) * w];
		tmp[3] = pixels[((h - 1) * w) + w - 1];

		int[] init = new int[4];
		int[] gHistogram = getRGBHistogram(pixels, 256)[COLOR_G];

		init[0] = (tmp[0] & 0x00ff00) >> 8;
		init[1] = (tmp[1] & 0x00ff00) >> 8;
		init[2] = (tmp[2] & 0x00ff00) >> 8;
		init[3] = (tmp[3] & 0x00ff00) >> 8;

		int gThreshold = getThreshold(gHistogram, init, 256);

		for (int y = 0; y < h; y++) {
			for (int x = 0; x < w; x++) {
				int c, r, g, b;
				c = pixels[(y * w) + x];
				r = (c & 0xff0000) >> 16;
				g = (c & 0x00ff00) >> 8;
				b = (c & 0x0000ff);

				if ((g < gThreshold)) {
					pixelnew[(y * w) + x] = (byte) 0;
				}
			}
		}

		byte[] tmpFrame = new byte[(w + 2) * (h + 2)];

		makeFrame(pixelnew, tmpFrame, w, h, 255);

		Vector<Polygon> polyvec = contour(tmpFrame.clone(), tmpFrame, w + 2, h + 2);

		removeFrame(tmpFrame, pixelnew, w + 2, h + 2);

		ij.IJ.log("" + polyvec.size());
		
		berechneShit(polyvec);
		
		// Polygon perfekten Kreis rausfinden
		// fuer jedes Polygonobjekt den Schwerpunkt rausfinden
		// fuer jeden Punkt der Kontur den Abstand zum Schwerpunkt bestimmen
		// daraus den Mittelwert -> das ist der Radius des Polygonobjektes
		// das wiederholt man fuer alle Konturen und hat am Ende also fuer jede Kontur einen Radius
		// wenn der kleiner ist als ein bestimmter Grenzwert, wird er ignoriert
		// damit werden nur die perfekten Radien (und damit Konturkreise) genommen
		// von denen wieder den Mittelwert bilden und wir haben einen perfekten Radius
		// daraus holt man den Durchmesser und daraus dann die perfekte Fläche
		// zum Schluss kann man also fuer jede Kontur ausrechnen:
		// nimm den radius der kontur -> berechne damit fläche der kontur
		// -> teile fläche durch perfekte fläche * 100 = Ergebnis ist wieviel Prozent der perfekten Kontur 

		// neues Bild anzeigen
		corrected.show();
		corrected.updateAndDraw();
	}

	// WAS NOCH FEHLT:
	// -die kleinen Konturschnipsel mit Laenge = 0 oder 1 weglassen irgendwie oder nicht beruecksichtigen?
	// -ordentliche Methodennamen und nicht immer mehrmals das gleiche berechnen
	// -vernueftige Ausgabe irgendwie?
	// -Irgendwas Schlaues mit den Grenzen fuer Radius und Abweichung, vllt berechnen?
	// -Histogrammegalisierung machen damit man alle Gewebestanzen findet
	/**
	 * Methode zum Berechnen von dem ganzen Ding
	 * 
	 * @param konturenvektor
	 */
	private void berechneShit(Vector<Polygon> konturenvektor) {
		
		double [] kandidaten = new double[konturenvektor.size()];
		double [] mittelradien = new double[konturenvektor.size()];
		
		// fuer alle Konturen den Radius speichern
		// und die guten Radien nochmal extra speichern, um daraus
		// einen perfekten Durchschnittsradius zu erhalten,
		// mit dem dann die Flaeche berechnet wird		
		for (int i = 0; i < konturenvektor.size(); i++) {
			Polygon konturobj = (Polygon) konturenvektor.elementAt(i);
			
			// eigentlich unnoetig, aber man braucht den Radius jeder Kontur spaeter noch fuer deren Flaechen...
			double mittlerer_rad = berechneMittlerenRad(konturobj);
			// deshalb hier ein Array, um fuer jede Kontur den Radius zu speichern
			mittelradien[i] = mittlerer_rad;
			
			double wert = berechneAlles(konturobj);
			kandidaten[i] = wert;
		}
		// Mittlerer Radius aller "guten" Radien ANFANG
		
		// "Perfekter" Radius ist der Mittelwert von allen Kandidatenradii
		double p_rad = 0;
		int count = 0;
		
		// merken, wieviele 0er-Radien dabei sind...
		for (int v = 0; v < kandidaten.length; v++) {
			p_rad += kandidaten[v];
			if (kandidaten[v] == 0) count++;
		}
		// ...um diese bei der Mittelwertberechnung auszuschliessen
		p_rad /= (kandidaten.length - count);
		IJ.log("Radius einer Stanze: " + p_rad);
		
		// Mittelwert aller "guten" Radien ENDE
		
		// "Perfekte" Flaeche dann nach der Kreisformel
		double p_flaeche = p_rad * p_rad * Math.PI;
		IJ.log("Flaeche einer Stanze: " + p_flaeche);
		
		// fuer jedes Konturobjekt...
		for (int t = 0; t < konturenvektor.size(); t++) {
			
			// ...nimm den mittleren Radius der Kontur,
			double rad = mittelradien[t];
			
			// ...berechne damit die Flaeche,
			double flaeche = rad * rad * Math.PI;
			
			// ...berechne damit den Anteil von der "Perfekten" Flaeche,
			// PROBLEM: bei manchen Flaechen kommt ein Wert groesser als 100 raus, weil sein Radius großer ist als der "perfekte" Durchschnittsradius
			double anteil = (flaeche / p_flaeche) * 100;
			
			// ... und zeig das Irgendwie an
			IJ.log("Flaeche von Kontur " + t + " entspricht " + anteil + " % der Stanzflaeche!");

		}
	}
	/**
	 * Hilfsmethode zum Finden des Schwerpunktes einer Kontur
	 * siehe http://stackoverflow.com/questions/18591964/how-to-calculate-centroid-of-an-arraylist-of-points
	 * 
	 * @param kontur Polygonobjekt
	 */
	private double berechneAlles(Polygon kontur) {

			// Schwerpunkt der Kontur ANFANG
			Punkt schwer = new Punkt(0,0);
			
			for (int j = 0; j < kontur.npoints; j++) {
				schwer.xKoord += kontur.xpoints[j];
				schwer.yKoord += kontur.ypoints[j];
			}
			schwer.xKoord /= kontur.npoints;
			schwer.yKoord /= kontur.npoints;
			// Schwerpunkt der Kontur ENDE
			
			// Mittlerer Radius der Kontur ANFANG
			double[] radien = new double[kontur.npoints];			
			Punkt linienpkt = new Punkt();
			double mittelrad = 0;
			
			for (int a = 0; a < kontur.npoints; a++) {
				linienpkt.xKoord = kontur.xpoints[a];
				linienpkt.yKoord = kontur.ypoints[a];
				double radius = berechneAbstand(schwer,linienpkt);
				mittelrad += radius;
				radien[a] = radius;
			}
			mittelrad = mittelrad / kontur.npoints;
			// Mittlerer Radius der Kontur ENDE
			
			// Mittlere Abweichung der Kontur ANFANG
			double abweichung = 0;
			for (int b = 0; b < kontur.npoints; b++) {
				abweichung += (radien[b] - mittelrad) * (radien[b] - mittelrad);
			}
			abweichung = Math.sqrt(abweichung / (kontur.npoints - 1));
			// Mittlere Abweichung der Kontur ENDE
			
			// Kandidaten finden
			if (mittelrad > RADIUS_GRENZE && abweichung < ABWEICHUNG_GRENZE) {
				//IJ.log(schwer.toString());
				//IJ.log("Mittlerer Radius: " + mittelrad);
				//IJ.log("Mittlerer Abweichung: " + abweichung);
				
				// "guter" Radius, kann man so zurueckgeben
				return mittelrad;
			} else {
				// Radius war "schlecht", also einfach 0 zurueck
				return 0;
			}	
	}
	/**
	 * Noch eine Hilfsmethode
	 * ist aber schon in berechneAlles() enthalten, muesste man mal sauber abgrenzen
	 * 
	 * @param kontur Polygonobjekt
	 * @return mittlerer Radius von kontur
	 */
	private double berechneMittlerenRad(Polygon kontur) {
		// Schwerpunkt der Kontur ANFANG
		Punkt schwer = new Punkt();
		
		for (int j = 0; j < kontur.npoints; j++) {
			schwer.xKoord += kontur.xpoints[j];
			schwer.yKoord += kontur.ypoints[j];
		}
		schwer.xKoord /= kontur.npoints;
		schwer.yKoord /= kontur.npoints;
		// Schwerpunkt der Kontur ENDE
		
		// Mittlerer Radius der Kontur ANFANG
		double[] radien = new double[kontur.npoints];			
		Punkt linienpkt = new Punkt();
		double mittelrad = 0;
		
		for (int a = 0; a < kontur.npoints; a++) {
			linienpkt.xKoord = kontur.xpoints[a];
			linienpkt.yKoord = kontur.ypoints[a];
			double radius = berechneAbstand(schwer,linienpkt);
			mittelrad += radius;
			radien[a] = radius;
		}
		mittelrad = mittelrad / kontur.npoints;
		// Mittlerer Radius der Kontur ENDE
		
		return mittelrad;
	}
	
	/**
	 * Hilfsmethode zur Abstandmessung
	 * 
	 * @param p1 Ein Punkt
	 * @param p2 Noch Ein Punkt
	 * @return Der euklidische Abstand zwischen den Punkten
	 */
	public double berechneAbstand (Punkt p1, Punkt p2) {
		return Math.sqrt(Math.pow(p2.xKoord-p1.xKoord, 2) + Math.pow(p2.yKoord-p1.yKoord, 2));
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////// Methoden zur Vorverarbeitung ANFANG
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Glaettung mit Mittelwertfilter
	 * 
	 * @param src Pixel-Array des Eingabebildes
	 * @param dest Pixel-Array des neuen Bildes
	 * @param w Breite des Eingabebildes
	 * @param h Hoehe des Eingabebildes
	 */
	public void smooth(int[] src, int[] dest, int w, int h) {
		
		for (int y = 1; y < h - 1; y++) {
			for (int x = 1; x < w - 1; x++) {
				int avg[] = new int[3];
				avg[COLOR_R] = 0;
				avg[COLOR_G] = 0;
				avg[COLOR_B] = 0;
				for (int i = 0; i < 3; i++) {
					for (int j = 0; j < 3; j++) {
						int tmp = src[((y + i - 1) * w) + x + j - 1];
						avg[COLOR_R] += (tmp & 0xff0000) >> 16;
						avg[COLOR_G] += (tmp & 0x00ff00) >> 8;
						avg[COLOR_B] += (tmp & 0x0000ff);
					}
				}

				avg[COLOR_R] /= 9;
				avg[COLOR_G] /= 9;
				avg[COLOR_B] /= 9;

				dest[(y * w) + x] = ((avg[COLOR_R] & 0xff) << 16) | ((avg[COLOR_G] & 0xff) << 8) | avg[COLOR_B] & 0xff;
			}
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////// Methoden zur Vorverarbeitung ENDE
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * 
	 * @param src
	 * @param depth
	 * @return
	 */
	private int[][] getRGBHistogram(int[] src, int depth) {
		int[][] rgbHistogram = new int[3][depth];
		for (int i = 0; i < src.length; i++) {
			int tmp = src[i];
			int rIntensity = (tmp & 0xff0000) >> 16;
			int gIntensity = (tmp & 0x00ff00) >> 8;
			int bIntensity = (tmp & 0x0000ff);
			rgbHistogram[COLOR_R][rIntensity]++;
			rgbHistogram[COLOR_G][gIntensity]++;
			rgbHistogram[COLOR_B][bIntensity]++;
		}
		return rgbHistogram;
	}

	/**
	 * 
	 * @param nums
	 * @param histogram
	 * @return
	 */
	private int avgFromList(List<Integer> nums, int[] histogram) {
		int sum = 0;
		int nNums = 0;
		if (nums.isEmpty()) {
			return 0;
		} else {
			for (int num : nums) {
				sum += num * histogram[num];
				nNums += histogram[num];
			}

			return sum / nNums;
		}

	}

	/**
	 * 
	 * @param histogram
	 * @param initValues
	 * @param depth
	 * @return
	 */
	private int getThreshold(int[] histogram, int[] initValues, int depth) {
		ArrayList<Integer> backgroundPixels = new ArrayList<Integer>();
		ArrayList<Integer> foregroundPixels = new ArrayList<Integer>();
		int thresholdOld = 0;
		int thresholdNew = 0;

		// Initialisierung
		for (int i = 0; i < initValues.length; i++) {
			backgroundPixels.add(initValues[i]);
		}

		for (int i = 0; i < depth; i++) {
			if (!backgroundPixels.contains(i))
				foregroundPixels.add(i);
		}

		do {
			thresholdOld = thresholdNew;
			int avgBackground = avgFromList(backgroundPixels, histogram);
			int avgForeground = avgFromList(foregroundPixels, histogram);

			thresholdNew = (avgForeground + avgBackground) / 2;

			backgroundPixels.clear();
			foregroundPixels.clear();

			for (int i = 0; i < thresholdNew; i++) {
				backgroundPixels.add(i);
			}

			for (int i = thresholdNew; i < depth; i++) {
				foregroundPixels.add(i);
			}

		} while (thresholdNew != thresholdOld);

		return thresholdNew;
	}

	/**
	 * 
	 * @param src
	 * @param dest
	 * @param w
	 * @param h
	 * @param value
	 */
	private void makeFrame(byte[] src, byte[] dest, int w, int h, int value) {
		for (int y = 0; y < (h + 2); y++) {
			for (int x = 0; x < (w + 2); x++) {
				if ((y == 0) || (x == 0) || (y == (h + 1)) || (x == (w + 1))) {
					dest[y * (w + 2) + x] = (byte) value;
				} else {
					dest[y * (w + 2) + x] = src[(y - 1) * w + (x - 1)];
				}
			}
		}
	}

	/**
	 * 
	 * @param src
	 * @param dest
	 * @param w
	 * @param h
	 */
	private void removeFrame(byte[] src, byte[] dest, int w, int h) {
		for (int y = 0; y < h; y++) {
			for (int x = 0; x < w; x++) {
				if (y == 0 || x == 0 || y == (h - 1) || x == (w - 1)) {

					continue;
				} else {
					dest[(y - 1) * (w - 2) + x - 1] = src[y * w + x];
				}
			}
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////// Methoden zum Erzeugen der Konturobjekte ANFANG 
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Methode zum Erzeugen der Konturobjekte (siehe Uebung)
	 * 
	 * @param src
	 * @param dest
	 * @param w
	 * @param h
	 * @return Vector, der alle Konturobjekte enthaelt
	 */
	private Vector<Polygon> contour(byte[] src, byte[] dest, int w, int h) {
		Vector<Polygon> polyvec = new Vector<Polygon>();

		for (int y = h - 1; y >= 0; y--) {
			for (int x = 0; x < w; x++) {
				dest[y * w + x] = (byte) 0;
				int pixel = src[(y * w) + x] & 0xff;
				
				if (pixel == 0) { // wenn Objektpixel...
					if (!checkcontains(x, y, polyvec)) { // und wenn noch nicht vorhanden in einer Kontur...
						createObject(src, src, x, y, w, polyvec); // erstelle neue Kontur
					}
				}
			}
		}

		for (Polygon p : (Polygon[]) polyvec.toArray(new Polygon[polyvec.size()])) {
			for (int i = 0; i < p.npoints; i++) {
				dest[p.ypoints[i] * w + p.xpoints[i]] = (byte) 255;
			}
		}

		return polyvec;
	}

	/**
	 * Hilfsmethode zum Erzeugen der Konturobjekte (siehe Uebung)
	 * 
	 * @param src
	 * @param dest
	 * @param x
	 * @param y
	 * @param w
	 * @param polyvec Vector zum Speichern aller Konturobjekte
	 */
	private void createObject(byte[] src, byte[] dest, int x, int y, int w, Vector<Polygon> polyvec) {
		Polygon polygon = new Polygon();
		int pixelX = x;
		int pixelY = y;
		int direction = 0; 
		
		// Hier Konturverfolgung solange, wie...
		do {
			int pixel = src[pixelY * w + pixelX] & 0xff;
			
			if (pixel == 0) { // wenn Objektpixel...
				
				polygon.addPoint(pixelX, pixelY);

				switch (direction) {
				case 0:
					pixelY--;
					break;
				case 1:
					pixelX++;
					break;
				case 2:
					pixelY++;
					break;
				case 3:
					pixelX--;
					break;
				}

				if (direction == 0) {
					direction = 3;
				} else {
					direction--;
				}
			} else { // wenn Hintergrundpixel...
				
				switch (direction) {
				case 0:
					pixelY++;
					break;
				case 1:
					pixelX--;
					break;
				case 2:
					pixelY--;
					break;
				case 3:
					pixelX++;
					break;
				}

				if (direction == 3) {
					direction = 0;
				} else {
					direction++;
				}
			}
			
		} while ((pixelX != x) || (pixelY != y)); // der Startwert nicht wieder erreicht ist

		// fertige Kontur hinzufuegen
		polyvec.addElement(polygon);
		//IJ.log("Kontur: " + polygon.npoints);
	}

	/**
	 * Hilfsmethode zum Erzeugen der Konturobjekte (siehe Uebung)
	 * Checks, if a point is inside one of the objects.
	 * 
	 * @param x
	 *            The x image coordinate of the point to be examined.
	 * @param y
	 *            the y image coordinate of the point to be examined.
	 * @return 
	 * 			  True, if the point lies inside an object, false otherwise.
	 */
	public boolean checkcontains(int x, int y, Vector<Polygon> polyvec) {
		for (int i = 0; i < polyvec.size(); i++) {
			Polygon poly = (Polygon) polyvec.elementAt(i);
			if (poly.contains(x, y))
				return true;

			for (int j = 0; j < poly.npoints; j++) {
				if (x == poly.xpoints[j] && y == poly.ypoints[j])
					return true;
			}
		}
		return false;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////// Methoden zum Erzeugen der Konturobjekte ENDE
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}

/**
 * Hilfsklasse fuer die Konturschwerpunkte
 * 
 * stellt einen Punkt mit einer X- und Y-Koordinate dar
 */
class Punkt {
	int xKoord;
	int yKoord;
	
	public Punkt() {
		this.xKoord = 0;
		this.yKoord = 0;
	}
	public Punkt(int x, int y) {
		this.xKoord = x;
		this.yKoord = y;
	}
	public Punkt (Punkt pt) {
		this.xKoord = pt.xKoord;
		this.yKoord = pt.yKoord;
	}
	public String toString() {
		return "(" + xKoord + "," + yKoord + ")";
	}
}
