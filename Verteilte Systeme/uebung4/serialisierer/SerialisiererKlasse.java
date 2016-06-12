package serialisierer;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Serialisierer <br>
 *
 * SAXParser <br>
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class SerialisiererKlasse {
	
	public static void starteSerialisierung(String data) {
		
        try {
	
        	// SAX Parser erstellen und Handler festlegen
            SAXParserFactory factory = SAXParserFactory.newInstance();
            SAXParser saxParser = factory.newSAXParser();
            DefaultHandler handler = new PersonenHandler();
            
            // XML String parsen und serialisieren
            saxParser.parse(new InputSource(new StringReader(data)), handler);
            
        } catch (ParserConfigurationException pce) {
            System.out.println("Parser Fehler!: " + pce.getMessage());
        } catch (SAXException saxe) {
            System.out.println("SAX Fehler!: " + saxe.getMessage());
        } catch (IOException ioe) {
            System.out.println("IO Fehler!: " + ioe.getMessage());
        }
	}
}

  

