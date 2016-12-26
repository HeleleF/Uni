package de.htw_berlin.ai_bachelor.kbe.checklist8.model;

import de.htw_berlin.ai_bachelor.kbe.checklist8.validierung.MyInterval;

import javax.validation.constraints.Future;
import java.io.Serializable;

import java.util.Calendar;
import java.util.Date;

public class ToDo implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private String _name;
	private boolean _done = false;
	
	@MyInterval(interval = "intervalMB.interval")
	private int _prioritaet;
	
	@Future()
	private Date _dueDate;
	
	private ToDo(String name, boolean done) {
		super();
		this._name = name;
		this._done = done;
		this._prioritaet = 1;
		Calendar c = Calendar.getInstance();
		c.add(Calendar.DATE, 1);
		this._dueDate = c.getTime();
	}
	
	public ToDo(String name) {
		this(name, false);
	}
	
	public boolean isDone() {
		return _done;
	}
	public void setDone(boolean done) {
		this._done = done;
	}
	public String getName() {
		return _name;
	}
	public void setName(String name) {
		this._name = name;
	}
	
	public Date getDueDate() {
		return _dueDate;
	}

	public void setDueDate(Date date) {
		this._dueDate = date;
	}

	public int getPrioritaet() {
		return _prioritaet;
	}

	public void setPrioritaet(int prioritaet) {
		this._prioritaet = prioritaet;
	}

}
