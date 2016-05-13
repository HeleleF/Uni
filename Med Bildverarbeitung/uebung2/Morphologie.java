package uebung2; //moeglicherweise auskommentieren, weil ImageJ damit nicht klarkommt

import ij.*;
import ij.gui.*;
import java.awt.*;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/** Morphologie <br>
 * 
 * Klasse zum Testen von Erosion und Dilation <br>
 * 
 * TODO: Javadockommentare schreiben <br>
 * TODO: Format
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class Morphologie implements PlugInFilter {

	/**Initialisierung in ImageJ */
	public int setup(String arg, ImagePlus imp) {
		if (arg.equals("about"))
			{showAbout(); return DONE;}
				//Zugelassen nur für 8-Bit Graubilder
                return DOES_8G+NO_CHANGES;
	}	

	void showAbout() {
		IJ.showMessage("Graubildtest",
			"Testprogramm"
		);
	}
	
	/**Ausführende Funktion
	 * @param ip Image Processor. Klasse in ImageJ, beinhaltet das Bild und 
	 * zugehörige Metadaten.
	 */
	public void run(ImageProcessor ip) {
		
		// get width, height and the region of interest
		int w = ip.getWidth();     
		int h = ip.getHeight();    
		Rectangle roi = ip.getRoi();

		// create a new image with the same size and copy the pixels of the original image
		ImagePlus corrected = NewImage.createByteImage ("Erosion", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor cor_ip = corrected.getProcessor();
		cor_ip.copyBits(ip,0,0,Blitter.COPY);
		
		ImagePlus corrected2 = NewImage.createByteImage ("Dilation", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor cor_ip2 = corrected2.getProcessor();
		cor_ip2.copyBits(ip,0,0,Blitter.COPY);
		
		//Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[])ip.getPixels();
		//Pixelarray des neuen Bildes
		byte[] pixels = (byte[])cor_ip.getPixels();
		byte[] pixels2 = (byte[])cor_ip2.getPixels();
		
		//Beispiel Filter
		for (int i=roi.y+1; i<roi.y+roi.height-1; i++) {
			for (int j=roi.x+1; j<roi.x+roi.width-1; j++) {
				
				//3x3 Faltungskern
				int pix1 = pixelsin[(i-1)*w+(j-1)];
				pix1 = (pix1&0x0000ff);
				int pix2 = pixelsin[(i-1)*w+j];
				pix2 = (pix2&0x0000ff);
				int pix3 = pixelsin[(i-1)*w+(j+1)];
				pix3 = (pix3&0x0000ff);
				int pix4 = pixelsin[i*w+(j-1)];
				pix4 = (pix4&0x0000ff);
				int pix5 = pixelsin[i*w+j];
				pix5 = (pix5&0x0000ff);
				int pix6 = pixelsin[i*w+(j+1)];
				pix6 = (pix6&0x0000ff);
				int pix7 = pixelsin[(i+1)*w+(j-1)];
				pix7 = (pix7&0x0000ff);
				int pix8 = pixelsin[(i+1)*w+j];
				pix8 = (pix8&0x0000ff);
				int pix9 = pixelsin[(i+1)*w+(j+1)];
				pix9 = (pix9&0x0000ff);

				//Erosion
				if (pix5 == 0 && (pix1 == 255 || pix2 == 255 
						|| pix3 == 255 || pix4 == 255 || pix6 == 255 
						|| pix7 == 255 || pix8 == 255 || pix9 == 255))
					pixels[i * w + j] = (byte) 255;
				
				//Dilation
				if (pix5 == 255 && (pix1 == 0 || pix2 == 0 
						|| pix3 == 0 || pix4 == 0 || pix6 == 0 
						|| pix7 == 0 || pix8 == 0 || pix9 == 0))
					pixels2[i * w + j] = (byte) 0;
			}
		}
		
		corrected.show();
		corrected.updateAndDraw();
		corrected2.show();
		corrected2.updateAndDraw();

	}
}



