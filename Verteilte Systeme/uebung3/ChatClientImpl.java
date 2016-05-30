
import java.rmi.*;
import java.rmi.server.UnicastRemoteObject;

/** ChatClientImpl <br>
*
* Implementierung des Clients <br>
* @author Chris Rebbelin s0548921
* @version 1.1
*/
@SuppressWarnings("serial")
public class ChatClientImpl extends UnicastRemoteObject implements ChatClient {

	/** Spitzname des Clients*/
	private String name;
	
	/**
	 * Konstruktor für einen Client
	 * 
	 * @param nick Spitzname des Clients
	 */
	public ChatClientImpl(String nick) throws RemoteException {
		name = nick;
	}
	
	@Override
	public String getName() throws RemoteException {
		return name;
	}	
	
	@Override
	public void print(String msg) throws RemoteException {
		System.out.println(msg);
	}
}
