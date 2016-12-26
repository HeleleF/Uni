package de.htw_berlin.ai_bachelor.kbe.checklist.mb;

import java.util.Date;

import javax.faces.bean.ApplicationScoped;
import javax.faces.bean.ManagedBean;

@ApplicationScoped
@ManagedBean (name = "datetime")
public class DateTimeMB {
			
	public DateTimeMB() {
	}
	
	public Date getNow() {
		return new Date();
	}
	
	public String goToIndex() {
		return "showTime";
	}

}
