package uebung4; //moeglicherweise auskommentieren, weil ImageJ damit nicht klarkommt

import java.awt.image.IndexColorModel;

import ij.*;
import ij.gui.*;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/**
 * LUT <br>
 *
 * Klasse zum Testen von Lookuptables <br>
 *
 * @author Chris Rebbelin s0548921
 * @version 1.1
 */
public class LUT implements PlugInFilter {
	
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

		// create a new image with the same size and copy the pixels of the
		// original image
		ImagePlus lookup = NewImage.createByteImage("lookup image", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor look_ip = lookup.getProcessor();
		look_ip.copyBits(ip, 0, 0, Blitter.COPY);

		// Beispiel Lookup Table LUT
		byte[] colorMapRed = new byte[256];
		byte[] colorMapGreen = new byte[256];
		byte[] colorMapBlue = new byte[256];

		//nach Aufgabenstellung
		//Rot geht von 255 bis 0 bei Ausgangsgrauwerten von 0 bis 127
		for (int i = 0; i < 128; i++)
			colorMapRed[i] = (byte) (255 - 2 * i);
		
		//Gruen geht von 0 bis 255 bei Ausgangsgrauwerten von 128 bis 255
		for (int i = 128; i <= 255; i++)
			colorMapGreen[i] = (byte) (2 * i - 255);
		
		//Blau geht von 0 bis 255 bei Ausgangsgrauwerten von 64 bis 127
		for (int i = 64; i < 128; i++)
			colorMapBlue[i] = (byte) (4 * i - 255);
		
		//Blau geht von 255 bis 0 bei Ausgangsgrauwerten von 128 bis 191
		for (int i = 128; i < 192; i++)
			colorMapBlue[i] = (byte) (767 - 4 * i);

		//LUT auf das Bild anwenden
		IndexColorModel cm = new IndexColorModel(8, 256, colorMapRed, colorMapGreen, colorMapBlue);
		look_ip.setColorModel(cm);

		lookup.show();
		lookup.updateAndDraw();

	}

}
