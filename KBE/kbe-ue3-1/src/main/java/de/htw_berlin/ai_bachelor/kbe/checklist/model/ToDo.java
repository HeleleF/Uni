package de.htw_berlin.ai_bachelor.kbe.checklist.model;

import java.io.Serializable;

public class ToDo implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private String name;
	private boolean done = false;
	
	private ToDo(String name, boolean done) {
		super();
		this.name = name;
		this.done = done;
	}
	
	public ToDo(String name) {
		this(name,false);
	}
	
	public boolean isDone() {
		return done;
	}
	public void setDone(boolean done) {
		this.done = done;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}

}
