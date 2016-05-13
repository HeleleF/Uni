package uebung2; //moeglicherweise auskommentieren, weil ImageJ damit nicht klarkommt

import ij.*;
import ij.gui.*;
import java.awt.*;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/** Morphologie
 * 
 * Klasse zum Testen von Schließung <br>
 * 
 * TODO: Javadockommentare <br>
 * TODO: Format <br>
 * TODO: Bug fixen fuer "Ganzes Bild wird weiß" 
 *  
 * @author Chris Rebbelin s0548921
 * @version 1.1
 */
public class Closing implements PlugInFilter {

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
		ImagePlus closing = NewImage.createByteImage ("Closing", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor close_ip = closing.getProcessor();
		close_ip.copyBits(ip,0,0,Blitter.COPY);

		//Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[])ip.getPixels();
		//Pixelarray des neuen Bildes
		byte[] pixels = (byte[])close_ip.getPixels();
		
		delate(pixelsin, pixels, roi, w, h);
	
		erode(pixels, pixels, roi, w, h);
		
		closing.show();
		closing.updateAndDraw();		
	}
	
	private void erode(byte[] pixelsin2, byte[] pixels, Rectangle roi, int w, int h){
		for (int i=roi.y+1; i<roi.y+roi.height-1; i++) {
			for (int j=roi.x+1; j<roi.x+roi.width-1; j++) {
				int pix1 = pixelsin2[(i-1)*w+(j-1)];
				pix1 = (pix1&0x0000ff);
				int pix2 = pixelsin2[(i-1)*w+j];
				pix2 = (pix2&0x0000ff);
				int pix3 = pixelsin2[(i-1)*w+(j+1)];
				pix3 = (pix3&0x0000ff);
				int pix4 = pixelsin2[i*w+(j-1)];
				pix4 = (pix4&0x0000ff);
				int pix5 = pixelsin2[i*w+j];
				pix5 = (pix5&0x0000ff);
				int pix6 = pixelsin2[i*w+(j+1)];
				pix6 = (pix6&0x0000ff);
				int pix7 = pixelsin2[(i+1)*w+(j-1)];
				pix7 = (pix7&0x0000ff);
				int pix8 = pixelsin2[(i+1)*w+j];
				pix8 = (pix8&0x0000ff);
				int pix9 = pixelsin2[(i+1)*w+(j+1)];
				pix9 = (pix9&0x0000ff);
				//erode
				if(pix5 == 0 && (pix1==255||pix2==255||pix3==255||pix4==255||pix6==255||pix7==255||pix8==255||pix9==255)) pixels[i*w+j] = (byte) 255;
			}
		}	
	}
	
	private void delate(byte[] pixelsin, byte[] pixels, Rectangle roi, int w, int h){
		for (int i=roi.y+1; i<roi.y+roi.height-1; i++) {
			for (int j=roi.x+1; j<roi.x+roi.width-1; j++) {
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
				//dilate
				if(pix5 == 255 && (pix1==0||pix2==0||pix3==0||pix4==0||pix6==0||pix7==0||pix8==0||pix9==0)) pixels[i*w+j] = (byte) 0;
			}
		}	
	}
}




