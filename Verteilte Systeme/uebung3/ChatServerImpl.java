
import java.rmi.*;
import java.rmi.server.*;
import java.util.*;

/** ChatServerImpl <br>
*
* Implementierung des Servers <br>
* @author Chris Rebbelin s0548921
* @version 1.1
*/
@SuppressWarnings("serial") 
public class ChatServerImpl extends UnicastRemoteObject implements ChatServer{
	
	/** Liste zum Speichern aller Clients*/
	private ArrayList<ChatClient> teilnehmer;
	
	/**
	 * Konstruktor für einen Server
	 */
	public ChatServerImpl() throws RemoteException {
		teilnehmer = new ArrayList<ChatClient>();
	}

	@Override
	public synchronized boolean addClient(ChatClient objRef) throws RemoteException {
		String name = objRef.getName();
		
		//ueber alle Clients iterieren
		for (Iterator<ChatClient> iter = teilnehmer.iterator(); iter.hasNext();) {
			ChatClient cc = iter.next();
			//schon vorhanden
			try {
				if(cc.getName().equals(name)) {
					System.out.println(cc.getName()+" konnte nicht angemeldet werden, da er schon existiert!");
					return false;
				}
			} catch (RemoteException exc) {
				iter.remove();
				System.out.println("Remotefehler! " + exc.getMessage());
			}
		}
		//noch nicht vorhanden
		System.out.println(objRef.getName()+" erfolgreich angemeldet!");
		teilnehmer.add(objRef);
		return true;
	}

	@Override
	public synchronized void removeClient(ChatClient objRef) throws RemoteException {
		System.out.println(objRef.getName()+" abgemeldet!");
		teilnehmer.remove(objRef);
		
	}

	@Override
	public synchronized void sendMessage(String name, String msg) throws RemoteException {
		
		for (Iterator<ChatClient> iter = teilnehmer.iterator(); iter.hasNext();) {
			ChatClient cc = iter.next();
			try {
				cc.print(name + ": " + msg);
			} catch (RemoteException exc) {
				iter.remove();
				System.out.println("Remotefehler! " + exc.getMessage());
			}
		}
	}	
}
