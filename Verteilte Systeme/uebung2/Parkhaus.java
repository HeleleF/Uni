//fuer Queue
import java.util.LinkedList;
import java.util.Queue;

/**
 * Ein Parkhaus mit einer Anzahl von Parkplaetzen
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.0
 */
public class Parkhaus {
	
	/** Anzahl der Parkplaetze */
	private int plaetze;
	/** Warteschlange fuer wartende Autos */
	private Queue<Auto> einlasskontrolle;
	
	/** Standard-Parkplatzgroesse */
	private final int MAXPLATZ = 10;
	
	/**
	 * Konstruktor für einen Parkplatz
	 * 
	 * @param plaetze Anzahl der Parkplaetze 
	 */
	public Parkhaus(int plaetze) {
		
		//nur bei sinnvoller Eingabe zuweisen, ansonsten Standard verwenden
		if (plaetze > 0) {
			this.plaetze = plaetze;
		} else {
			System.out.println("Fehlerhafter Input! Standardwert verwendet!");
			this.plaetze = MAXPLATZ;
		}
		
		//Queue, in die "Autos" aufgenommen werden koennen
		einlasskontrolle = new LinkedList<Auto>();
	}

	/**
	 * Methode zum Einfahren eines Autos in das Parkhaus
	 * 
	 * @param auto Ein einfahrendes Auto
	 */
	public synchronized void reinfahren(Auto auto) {
		
		//Auto der Queue hinzufuegen
		einlasskontrolle.add(auto);
		System.out.println(auto.getAutoname() + " moechte einfahren!");
		
		//solange keine Plaetze frei ODER
		//Auto nicht an erster Stelle wartet, warten
		while (plaetze == 0 || auto != einlasskontrolle.element()) {
			try {
				wait();
			} catch (InterruptedException e) {
			}
		}
		
		//Erstes Auto aus der Queue entfernen
		einlasskontrolle.remove(auto);
		
		//Ein Platz wird belegt, Anzahl senken
		plaetze--;
		System.out.println(auto.getAutoname() + " faehrt ins Parkhaus! Freie Plaetze:" + plaetze);
	}

	/**
	 * Methode zum Ausfahren eines Autos aus dem Parkhaus
	 * 
	 * @param auto Ein ausfahrendes Auto
	 */
	public synchronized void rausfahren(Auto auto) {
		
		//Ein Platz wird frei, Anzahl erhoehen
		plaetze++;
		System.out.println(auto.getAutoname() + " verlaesst das Parkhaus! Freie Plaetze:" + plaetze);
		
		//wartenden Autos Bescheid geben
		notifyAll();
	}
}
