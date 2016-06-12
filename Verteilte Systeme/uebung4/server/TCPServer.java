package server;

import java.net.*;
import java.io.*;

import server.Connection;

/** TCPServer <br>
*
* Ein TCP-Server <br>
* 
* @author Chris Rebbelin s0548921
* @version 1.2
*/
public class TCPServer {

	/** Portnummer */
	final static int PORT = 9876;
	
	/** Serversocket */
	private static ServerSocket listenSocket;

	/**
	 * main Methode
	 * 
	 * @param args Stringarray, ungenutzt
	 */
	public static void main(String[] args) {
		try {
			// Serversocket erstellen...
			listenSocket = new ServerSocket(PORT);
			
			System.out.println("Server lauscht auf Port "+PORT);
			
			// und auf Verbindung warten...
			while (true) {
				Socket clientSocket = listenSocket.accept();
				new Connection(clientSocket);
			}

		} catch (IOException ioe) {
			System.out.println("IO-Fehler!: " + ioe.getMessage());
		}
	}
}
