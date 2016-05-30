
import java.rmi.*;

/** ChatServer <br>
*
* Schnittstelle des Servers <br>
* 
* @author Chris Rebbelin s0548921
* @version 1.1
*/
public interface ChatServer extends Remote {

	/**
	 * Client hinzufuegen
	 * 
	 * @param objRef Ein ChatClient
	 * @return TRUE, wenn Client erfolgreich hinzugefuegt, sonst FALSE
	 */
	public boolean addClient(ChatClient objRef) throws RemoteException;

	/**
	 * Client entfernen
	 * 
	 * @param objRef Ein ChatClient
	 */
	public void removeClient(ChatClient objRef) throws RemoteException;

	/**
	 * Übermittlung des Redebeitrags durch den Server
	 * 
	 * @param name Name des Clients
	 * @param msg Redebeitrag
	 */
	public void sendMessage(String name, String msg) throws RemoteException;

}
