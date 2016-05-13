package uebung2; //moeglicherweise auskommentieren, weil ImageJ damit nicht klarkommt

import ij.*;
import ij.gui.*;
import java.awt.*;
import ij.plugin.filter.PlugInFilter;
import ij.process.*;

/** Punktoperator <br>
 * 
 * Klasse zum Testen von Punktoperatoren <br>
 * 
 * TODO: Javadockommentare schreiben <br>
 * TODO: Format
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.1
 */
public class Punktoperator implements PlugInFilter {

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

	public void run(ImageProcessor ip) {
		
		// get width, height and the region of interest
		int w = ip.getWidth();     
		int h = ip.getHeight();    
		Rectangle roi = ip.getRoi();
		
		ImagePlus corrected3 = NewImage.createByteImage ("Laplace Operator", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor cor_ip3 = corrected3.getProcessor();
		cor_ip3.copyBits(ip,0,0,Blitter.COPY);
		
		ImagePlus corrected4 = NewImage.createByteImage ("Sobel X Operator", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor cor_ip4 = corrected4.getProcessor();
		cor_ip4.copyBits(ip,0,0,Blitter.COPY);
		
		ImagePlus corrected5 = NewImage.createByteImage ("Sobel Y Operator", w, h, 1, NewImage.FILL_BLACK);
		ImageProcessor cor_ip5 = corrected5.getProcessor();
		cor_ip5.copyBits(ip,0,0,Blitter.COPY);
		
		//Pixel-Array des Eingabebildes
		byte[] pixelsin = (byte[])ip.getPixels();
		//Pixelarray des neuen Bildes
		byte[] pixels3 = (byte[])cor_ip3.getPixels();
		byte[] pixels4 = (byte[])cor_ip4.getPixels();
		byte[] pixels5 = (byte[])cor_ip5.getPixels();
		
		//Beispiel Filter
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

				//Laplace-Operator
				int newpix1 = 127 + (-pix2 - pix4 + 4*pix5 -pix6 - pix8);	
				//Sobel-x Operator
				int newpix2 = 127 + pix1+2*pix2+pix3-pix7-2*pix8-pix9;
				//Sobel-y Operator
				int newpix3 = 127 + pix1+2*pix4+pix7-pix3-2*pix6-pix9;

				pixels3[i*w+j] = (byte) (ueberlauf(newpix1));
				pixels4[i*w+j] = (byte) (ueberlauf(newpix2));
				pixels5[i*w+j] = (byte) (ueberlauf(newpix3));
				
			}
		}
		

		corrected3.show();
		corrected3.updateAndDraw();
		corrected4.show();
		corrected4.updateAndDraw();
		corrected5.show();
		corrected5.updateAndDraw();
	}
	
	private int ueberlauf(int pixl) {
		if(pixl < 0) pixl = 0;
		else if (pixl > 255) pixl = 255;
		return pixl;
	}
}



