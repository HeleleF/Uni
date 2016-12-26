package de.htw_berlin.ai_bachelor.kbe.checklist8.mb;

import java.io.Serializable;
import javax.faces.bean.ManagedBean; 
import javax.faces.bean.SessionScoped;

import de.htw_berlin.ai_bachelor.kbe.checklist8.model.ToDoList;

// TodoList wird zur besseren Wiederverwendbarkeit referenziert von der ManagedBean
// so kann die Klasse TodoList immer noch fuer anderes verwendet werden
@SessionScoped
@ManagedBean
public class ToDoListMB implements Serializable {

	private static final long serialVersionUID = 1L;

    private final ToDoList toDoList;
    
	public ToDoListMB() {
		super();
		toDoList = new ToDoList();	
	}    

	public ToDoList getToDoList() {
		return toDoList;
	}

	//Should be called if the Button "Speichern" is pushed.
	//Needs configuration in the faces-config.xml.
	// Controller erweitert theoretisch
    public String save() {
    	return "save";
    }
    
	public String cancel() {
		return "cancel";
	}
    
	public String editInterval() {
		return "editInterval";
	}
}
