
import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.RemoteException;

/** ServerStart <br>
*
* Server <br>
* 
* @author Chris Rebbelin s0548921
* @version 1.1
*/
public class ServerStart {

	public static void main(String[] args) {
		try {
			//Instanzierung Server
			ChatServerImpl server = new ChatServerImpl();
			Naming.rebind("ChatServer", server);
			System.out.println("ChatServer läuft!");
			
		} catch (RemoteException rem) {
			System.out.println("Remotefehler! " + rem.getMessage());
		} catch (MalformedURLException mfu) {
			System.out.println("Serveradresse fehlerhaft! " + mfu.getMessage());
		}
	}
}
