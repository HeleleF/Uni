/**
 * Parkhausverwalter zum Testen 
 * von Autos und Parkhaus 
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.0
 */
public class ParkhausVerwalter {
	
	/** main Methode */
	public static void main(String[] args) {
		
		//Parkhaus mit 3 Parkplaetzen erzeugen
		Parkhaus parkhaus = new Parkhaus(3);
		
		//5 Auto-Threads erzeugen und "starten"
		for (int i = 0; i < 5; i++) {
			Auto fahrer = new Auto(parkhaus, "Auto" + i);
			fahrer.start();
		}
	}
}

