/**
 * Ein Auto mit einem Namen 
 * und einem zugehoerigem Parkhaus
 * 
 * @author Chris Rebbelin s0548921
 * @version 1.0
 */
public class Auto extends Thread {
	
	/** Parkhaus, in dem das Auto parken will */
	private Parkhaus parkhaus;
	/** Autoname zur Identifizierung */
	private String autoname;
	
	/** Fahrzeit in Millisekunden */
	private final int FAHRZEIT = 3000;
	/** Parkzeit in Millisekunden */
	private final int PARKZEIT = 8000;

	/**
	 * Konstruktor für ein Auto
	 * 
	 * @param parkhaus Parkhaus, in dem das Auto parken will
	 * @param name Identifizierung des Autos
	 */
	public Auto(Parkhaus parkhaus, String name) {
		this.parkhaus = parkhaus;
		this.autoname = name;	
	}

	/** 
	 * @see java.lang.Thread#run()
	 */
	public void run() {
		while (true) { //Autos fahren staendig ein und aus
			
			//simuliert Anfahrtszeit des Autos zum Parkhaus
			try {
				sleep((int) (Math.random() * FAHRZEIT));
			} catch (InterruptedException e) {
			}
			
			//Auto faehrt ins Parkhaus
			parkhaus.reinfahren(this);
			
			//simuliert Parkzeit des Autos
			try {
				sleep((int) (Math.random() * PARKZEIT));
			} catch (InterruptedException f) {
			}
			
			//Auto verlaesst Parkhaus
			parkhaus.rausfahren(this);
		}
	}

	/**
	 * Getter-Methode fuer den Namen des Autos
	 * @return "Name" des Autos
	 */
	public String getAutoname() {
		return autoname;
	}
}

