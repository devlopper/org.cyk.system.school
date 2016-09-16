package org.cyk.system.school.ui.web.primefaces.integration;

import org.cyk.system.school.ui.web.primefaces.session.StudentWebITRunner;

public class StudentWebIT extends AbstractWebIT {

	private static final long serialVersionUID = 1L;
	
	public StudentWebIT() {
		runnables.add(new StudentWebITRunner());
	}
	   
}
