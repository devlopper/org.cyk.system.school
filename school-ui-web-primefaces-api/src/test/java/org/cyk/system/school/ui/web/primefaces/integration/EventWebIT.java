package org.cyk.system.school.ui.web.primefaces.integration;

import org.cyk.ui.web.primefaces.test.automation.event.EventWebITRunner;

public class EventWebIT extends AbstractWebIT {

	private static final long serialVersionUID = 1L;
	
	public EventWebIT() {
		runnables.add(new EventWebITRunner());
	}
	   
}
