package uebung4; //moeglicherweise auskommentieren, weil ImageJ damit nicht klarkommt

import java.awt.Rectangle;

import java.lang.Math;

import ij.*;
import ij.gui.*;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/**
 * Transformation <br>
 *
 * Klasse zum Testen von Rotation <br>
 *
 * @author Chris Rebbelin s0548921
 * @version 1.1
 */
public class Transformation implements PlugInFilter {
	
	/**Initialisierung in ImageJ */
	public int setup(String arg, ImagePlus imp) {
		if (arg.equals("about"))
			{showAbout(); return DONE;}
				//Zugelassen nur für 8-Bit Graubilder
                return DOES_8G+NO_CHANGES;
	}	
	/**About Message zu diesem Plug-In. */
	void showAbout() {
		IJ.showMessage("Graubildtest",
			"Testprogramm"
		);
	}
	
	/**
	 * Ausführende Funktion
	 * 
	 * @param ip
	 *            Image Processor. Klasse in ImageJ, beinhaltet das Bild und
	 *            zugehörige Metadaten.
	 */
	public void run(ImageProcessor ip) {

		// get width, height and the region of interest
		int w = ip.getWidth();
		int h = ip.getHeight();
		Rectangle roi = ip.getRoi();

		// create a new image with the same size and copy the pixels of the
		// original image
		ImagePlus rotated = NewImage.createByteImage("rotated image", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor rotate_ip = rotated.getProcessor();
		rotate_ip.copyBits(ip, 0, 0, Blitter.COPY);

		// Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[]) ip.getPixels();
		// Pixelarray des neuen Bildes
		byte[] pixels = (byte[]) rotate_ip.getPixels();

		//Winkel in rad
		double alpha = -30.0;
		double winkel = Math.PI / 180.0 * alpha;
		
		//ueber alle Bildpixel iterieren
		for (int i = roi.y; i < roi.y + roi.height; i++) {
			for (int j = roi.x; j < roi.x + roi.width; j++) {
				
				//Drehung um Bildmittelpunkt, d.h. Offset, um Drehpunkt in den Ursprung zurueck zu setzen
				int widOff = roi.width / 2;
				int heiOff = roi.height / 2;

				// siehe Roationsformeln:
				// xNeu = x1 + cos(x-x1) - sin(y-y1)
				// yNeu = y1 + sin(x-x1) + cos(y-y1)
				
				int xn = widOff + (int) Math.round(Math.cos(winkel) * (j - widOff) - Math.sin(winkel) * (i - heiOff));
				int yn = heiOff + (int) Math.round(Math.sin(winkel) * (j - widOff) + Math.cos(winkel) * (i - heiOff));

				//neue Koordinaten auf Grenzenueberschreitung pruefen
				if (xn >= 0 && xn < w && yn >= 0 && yn < h) {
					pixels[i * w + j] = (byte) pixelsin[yn * w + xn];
				} else {
					pixels[i * w + j] = (byte) 0;
				}
			}
		}

		rotated.show();
		rotated.updateAndDraw();

	}
}
