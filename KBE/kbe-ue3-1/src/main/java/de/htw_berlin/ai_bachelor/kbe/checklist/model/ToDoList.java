package de.htw_berlin.ai_bachelor.kbe.checklist.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


public class ToDoList implements Serializable {
	

	private static final long serialVersionUID = 1L;
	
	private List<ToDo> toDos =  new ArrayList<ToDo>();
	
	
	public ToDoList() {
		super();
		this.setToDos();
	}
		
	public List<ToDo> getToDos() {
		return toDos;
	}
	
	//TODO Anzahl der Todos (g)
	public int getAnzahl() {
		return toDos.size();
	}
	
	//TODO Anzahl der erledigten Todos (g)
	public int getAnzahlDone() {
		int anzahlDone = 0;
		for (ToDo td : toDos) {
			if (td.isDone()) anzahlDone++;
		}
		return anzahlDone;
	}

	private void setToDos() {
		toDos.add(new ToDo("KBE: Aufgabenzettel 2 bearbeiten"));
		toDos.add(new ToDo("KBE: Zweite Vorlesung nacharbeiten"));
		toDos.add(new ToDo("Lebensmittel einkaufen "));
		toDos.add(new ToDo("Leergut wegbringen"));
		toDos.add(new ToDo("Geburtstagsgeschenk besorgen"));
		toDos.add(new ToDo("Putzen"));
	}
}
