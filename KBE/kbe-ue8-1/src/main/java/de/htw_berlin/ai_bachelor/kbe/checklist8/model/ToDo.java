package de.htw_berlin.ai_bachelor.kbe.checklist8.model;

import de.htw_berlin.ai_bachelor.kbe.checklist8.validierung.MyInterval;

import javax.validation.constraints.Future;
import java.io.Serializable;

import java.util.Calendar;
import java.util.Date;

public class ToDo implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private String name;
	private boolean done = false;
	
	@MyInterval
	private int prioritaet;
	
	@Future()
	private Date dueDate;
	
	private ToDo(String name, boolean done) {
		super();
		this.name = name;
		this.done = done;
		this.prioritaet = 1;
		
		Calendar c = Calendar.getInstance();	
		c.add(Calendar.DATE, 1);		
		this.dueDate = c.getTime();
	}
	
	public ToDo(String name) {
		this(name, false);
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
	
	public Date getDueDate() {
		return dueDate;
	}

	public void setDueDate(Date date) {
		dueDate = date;
	}

	public int getPrioritaet() {
		return prioritaet;
	}

	public void setPrioritaet(int prioritaet) {
		this.prioritaet = prioritaet;
	}

}
