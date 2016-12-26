package de.htw_berlin.ai_bachelor.kbe.checklist8.model;

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
	
	public int getAnzahl() {
		return toDos.size();
	}
	
	public int getAnzahlDone() {
		int anzahlDone = 0;
		for (final ToDo td : toDos) { //Todo td ist ja Iterator und sollte also nicht veraendert werden, deshalb final
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
