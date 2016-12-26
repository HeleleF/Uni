package de.htw_berlin.ai_bachelor.kbe.checklist;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

public class MeinCustomPhaseListener implements PhaseListener {

	private static final long serialVersionUID = 1L;
	
	// KomponentenFamilie / deren Anzahl
	private HashMap<String, Integer> mapf;
	
	// Anzahl aller Komponenten gesamt
	private int anzahl = 0;

	@Override
	public void afterPhase(PhaseEvent event) {
		
		PhaseId pid = event.getPhaseId();
		
		System.out.println("ENDE VON PHASE: " + pid.getName());
		
		if (pid == PhaseId.RESTORE_VIEW || pid == PhaseId.RENDER_RESPONSE) {
			
			// zuruecksetzen
			mapf = new HashMap<>();
			anzahl = 0;
			
			FacesContext context = event.getFacesContext();
				
			// falls RESTORE_VIEW, ist es ein Postback Request JA / NEIN ?
			if (pid == PhaseId.RESTORE_VIEW)
				System.out.println("POSTBACK REQUEST: " + context.isPostback());
					
			// Wurzel des Komponentenbaumes holen und ausgeben
			UIViewRoot wurzel = context.getViewRoot();			
			System.out.println("Wurzel: " + wurzel.getId() + " (" + wurzel.getClass().getName() + ")");	
			
			// Rekursiv alle Komponenten des Baumes zählen und Anzahl ausgeben
			rekursivePhase(wurzel);
			System.out.println("Anzahl Kinder: " + anzahl);		
			
			// falls RENDER_RESPONSE, die Komponentenfamilien inkl. Anzahl ausgeben
			if (pid == PhaseId.RENDER_RESPONSE) {
			    // siehe http://stackoverflow.com/questions/5920135/printing-hashmap-in-java
			    for (final Map.Entry<String, Integer> entry : mapf.entrySet()) {
			        System.out.println(entry.getKey() + ": " + entry.getValue());
			    }
			}
		}	
		System.out.println("");	
	}
	
	/**
	 * Rekursiv alle Komponten zaehlen
	 * und deren Familien in HashMap speichern
	 */
	public void rekursivePhase(UIComponent ui) {
		
		// Familie der momentanen Komponente holen
		// und Anzahl in der HashMap entweder updaten
		// oder initial auf 1 setzen 
    	String mom = ui.getFamily();
    	int cnt = mapf.getOrDefault(mom, 0);
    	mapf.put(mom, cnt + 1);
    	
    	// Anzahl der Komponenten gesamt erhoehen
    	anzahl++;
    	
    	// falls die Komponente Kinder hat (Abbruchbedingung -> null)
    	// fuer alle die Funktion rekurisv aufrufen
    	List<UIComponent> akku = ui.getChildren();
    	if (akku != null) {
    		for (final UIComponent rekui: akku) {
    			rekursivePhase(rekui);
    		}
    	}
	}
 	

	@Override
	public void beforePhase(PhaseEvent event) {
		System.out.println("START VON PHASE: " + event.getPhaseId().getName());
		
	}

	@Override
	public PhaseId getPhaseId() {
		return PhaseId.ANY_PHASE;
	}

}
