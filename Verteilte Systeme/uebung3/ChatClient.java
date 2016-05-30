
import java.rmi.*;

/** ChatClient <br>
*
* Schnittstelle des Clients <br>
* 
* @author Chris Rebbelin s0548921
* @version 1.1
*/
public interface ChatClient extends Remote {

	/**
	 * Server kann Name des Clients erfragen
	 * 
	 * @return Nickname des Clients
	 */
	public String getName() throws RemoteException;
	
	/**
	 * Übermittlung des Redebeitrags durch den Server
	 * 
	 * @param msg Redebeitrag
	 */
	public void print(String msg) throws RemoteException;

}
