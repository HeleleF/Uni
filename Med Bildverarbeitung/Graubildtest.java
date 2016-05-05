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
  * @version 1.1
  */
public class Graubildtest implements PlugInFilter {

	/**Initialisierung in ImageJ */
	public int setup(String arg, ImagePlus imp) {
		if (arg.equals("about"))
			{showAbout(); return DONE;}
				//Zugelassen nur f�r 8-Bit Graubilder
                return DOES_8G+NO_CHANGES;
	}	
	/**About Message zu diesem Plug-In. */
	void showAbout() {
		IJ.showMessage("Graubildtest",
			"Testprogramm"
		);
	}

	/**Ausf�hrende Funktion
	 * @param ip Image Processor. Klasse in ImageJ, beinhaltet das Bild und 
	 * zugeh�rige Metadaten.
	 * 	 */
	public void run(ImageProcessor ip) {
		
		// get width, height and the region of interest
		int w = ip.getWidth();     
		int h = ip.getHeight();    
		Rectangle roi = ip.getRoi();

		// neues Bild erzeugen f�r Invertierung
		ImagePlus inverted = NewImage.createByteImage ("Invertiert", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor inverted_ip = inverted.getProcessor();
		inverted_ip.copyBits(ip,0,0,Blitter.COPY);
		
		//Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[])ip.getPixels();
		
		//Pixelarray des invertierten Bildes
		byte[] pixels = (byte[])inverted_ip.getPixels();
		
		/***********An dieser Stelle kann an den einzelnen Pixeln gearbeitet werden.*********/
			
//		//Bildausschnitt invertieren �bung 1 
//		for (int zeile = roi.y; zeile < roi.y + roi.height; zeile++) { 			//�ber Zeilen iterieren
//			
//			for (int spalte = roi.x; spalte < roi.x + roi.width; spalte++) { 	//�ber Spalten jeder Zeile iterieren
//				
//				int position = zeile * w + spalte; 		//Pixel an Stelle (zeile,spalte)
//				int wert = pixelsin[position];
//				wert = wert&0xff; 						//Verschieben wegen Big/Little Endian
//				
//				/** �bung 1 Invertieren **/
//				pixels[position] = (byte) (255 - wert); 			
//				/** �bung 1 Invertieren **/
//				
////				/** �bung 1 Graustufen **/		
////				if (wert <= 42)
////						pixels[position] = (byte) 0;
////				if (wert > 42 && wert <= 127) 
////						pixels[position] = (byte) 85;
////				if (wert > 127 && wert <= 192)
////						pixels[position] = (byte) 170;
////				if (wert > 192) 
////						pixels[position] = (byte) 255;
////				/** �bung 1 Graustufen **/
//				}
//		}			
		
				/** �bung 1 Aufl�sung **/
				
		for (int zeile = roi.y; zeile < roi.y + roi.height; zeile+=2) { 			
			for (int spalte = roi.x; spalte < roi.x + roi.width; spalte+=2) {
				
				int position = zeile * w + spalte;
				
				int wert1 = pixelsin[position];
				int wert2 = pixelsin[position+1];
				int wert3 = pixelsin[position+w];
				int wert4 = pixelsin[position+w+1];
				
				wert1 = wert1&0xff;
				wert2 = wert2&0xff;
				wert3 = wert3&0xff;
				wert4 = wert4&0xff;
				
				pixels[position] = (byte) ((wert1+wert2+wert3+wert4)/4);
				
				}
		}
				
				/** �bung 1 Aufl�sung **/
				
				

		
		
		/*****************Ende**********************************************************/
		
		inverted.show();
		inverted.updateAndDraw();
		
	}
}


