
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

/** StartClient <br>
*
* Client <br>
* 
* @author Chris Rebbelin s0548921
* @version 1.1
*/
public class ClientStart {

	public static void main(String[] args) {
		
		//nach Aufgabenstellung
		if (args.length != 2) {
			System.out.println("2 Argumente nötig! Sytax: <Serveradresse> <ClientName>");
			System.exit(1);
		}

		try {
			//Instanzierung Client
			ChatClientImpl client = new ChatClientImpl(args[1]);
			ChatServer server = (ChatServer) Naming.lookup("rmi://" + args[0] + "/ChatServer");
			
			//wenn Client erfolgreich hinzugefuegt werden konnte...
			if (server.addClient(client) == true) {
				//nach Aufgabenstellung
				BufferedReader buf = new BufferedReader(new InputStreamReader(System.in));
				//...warte in Endlosschleife zum Eingeben von Text...
				while (true) {
					//Zeile einlesen und an alle abschicken
					//wenn "logout" geschrieben wird, abmelden und beenden
					String s = buf.readLine();
					if ("logout".equalsIgnoreCase(s)) {
						server.removeClient(client);
						System.exit(0);
					}
					server.sendMessage(args[1], s);
				}
			} else { //...ansonsten verlassen
				System.exit(1);
			}
		} catch (RemoteException rem) {
			System.out.println("Remotefehler! " + rem.getMessage());
		} catch (MalformedURLException mfu) {
			System.out.println("Serveradresse fehlerhaft! " + mfu.getMessage());
		} catch (NotBoundException nbe) {
			System.out.println("Server laueft nicht! " + nbe.getMessage());
		} catch (IOException ioe) {
			System.out.println("Inputfehler! " + ioe.getMessage());
		}
	}
}
