import ij.*;
import ij.gui.*;
import java.awt.*;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/** Graubildtest
  *
  * Klasse zum Testen von BV-Algorithmen in Graubildern
  *
  * @author Chris Rebbelin s0548921
  * @version 1.2
  */
public class Graubildtest implements PlugInFilter {

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

	/**Ausführende Funktion
	 * @param ip Image Processor. Klasse in ImageJ, beinhaltet das Bild und 
	 * zugehörige Metadaten.
	 * 	 */
	public void run(ImageProcessor ip) {
		
		// get width, height and the region of interest
		int w = ip.getWidth();     
		int h = ip.getHeight();    
		Rectangle roi = ip.getRoi();

		// neues Bild erzeugen für Invertierung
		ImagePlus inverted = NewImage.createByteImage ("Uebung1", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor inverted_ip = inverted.getProcessor();
		inverted_ip.copyBits(ip,0,0,Blitter.COPY);
		
		//Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[])ip.getPixels();
		
		//Pixelarray des invertierten Bildes
		byte[] pixels = (byte[])inverted_ip.getPixels();
		
		/***********An dieser Stelle kann an den einzelnen Pixeln gearbeitet werden.*********/
			
		for (int zeile = roi.y; zeile < roi.y + roi.height; zeile++) { 			//über Zeilen iterieren
			for (int spalte = roi.x; spalte < roi.x + roi.width; spalte++) { 	//über Spalten jeder Zeile iterieren
				
				int position = zeile * w + spalte; 		//Pixel an Stelle (zeile,spalte)
				int wert = pixelsin[position];
				wert = wert&0x0000ff; 						//Verschieben wegen Big/Little Endian

//				/** Übung 1 Invertieren **/
//				pixels[position] = (byte) (255 - wert); 			
//				/** Übung 1 Invertieren **/
				
				/** Übung 1 Graustufen **/		
				if (wert <= 42)
						pixels[position] = (byte) 0;
				if (wert > 42 && wert <= 127) 
						pixels[position] = (byte) 85;
				if (wert > 127 && wert <= 192)
						pixels[position] = (byte) 170;
				if (wert > 192) 
						pixels[position] = (byte) 255;
				/** Übung 1 Graustufen */
				}
		}			
		

//		/** Übung 1 Auflösung **/		
//		for (int zeile = roi.y+1; zeile < roi.y + roi.height; zeile+=2) { 			
//			for (int spalte = roi.x+1; spalte < roi.x + roi.width; spalte+=2) {
//				
//				int position = zeile * w + spalte;
//				
//				int wert1 = pixelsin[position-w-1];
//				int wert2 = pixelsin[position-w];
//				int wert3 = pixelsin[position-1];
//				int wert4 = pixelsin[position];
//				
//				wert1 = wert1&0x0000ff;
//				wert2 = wert2&0x0000ff;
//				wert3 = wert3&0x0000ff;
//				wert4 = wert4&0x0000ff;
//				
//				int _wert = (wert1+wert2+wert3+wert4)/4;
//				
//				pixels[position-w-1] = (byte) _wert;
//				pixels[position-w] = (byte) _wert;
//				pixels[position-1] = (byte) _wert;
//				pixels[position] = (byte) _wert;
//				
//				}
//		}
//		/** Übung 1 Auflösung **/		

		/*****************Ende**********************************************************/
		
		inverted.show();
		inverted.updateAndDraw();
		
	}
}


