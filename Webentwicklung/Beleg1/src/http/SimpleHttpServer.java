package http;

import java.net.InetSocketAddress;
import com.sun.net.httpserver.HttpServer;

/**
 * SimpleHttpServer <br>
 * 
 * Ein einfacher Webserver
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.2
 */
public class SimpleHttpServer {

	// Konstanten
	
	/** Portnummer */
	final static int PORT = 9000;

	/**
	 * main-Methode zum Starten des Servers
	 * 
	 * @param args Wird nicht verwendet
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {

		// Server und Kontext erstellen
		HttpServer server = HttpServer.create(new InetSocketAddress(PORT), 0);
		server.createContext("/index.html", new HandlerFuerAlles());
		server.createContext("/css/content.css", new HandlerFuerAlles());
		server.createContext("/images/Calculate.svg", new HandlerFuerAlles());
		server.createContext("/images/headerdot.svg", new HandlerFuerAlles());
		server.createContext("/images/headerdotDown.svg", new HandlerFuerAlles());
		server.createContext("/images/help.svg", new HandlerFuerAlles());
		server.createContext("/images/Projectdata.svg", new HandlerFuerAlles());
		server.createContext("/images/Reset.svg", new HandlerFuerAlles());
		server.createContext("/images/RotamassAll.svg", new HandlerFuerAlles());

		// the default executor
		server.setExecutor(null); 

		// starten
		server.start(); 
	}
}