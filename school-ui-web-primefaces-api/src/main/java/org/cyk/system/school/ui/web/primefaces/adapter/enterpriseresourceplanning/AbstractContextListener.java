package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;

import javax.servlet.ServletContextEvent;

import org.cyk.system.company.ui.web.primefaces.CompanyWebManager;
import org.cyk.system.root.ui.web.primefaces.api.RootWebManager;
import org.cyk.system.school.ui.web.primefaces.AbstractSchoolContextListener;

public abstract class AbstractContextListener extends AbstractSchoolContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	@SuppressWarnings({ })
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
			
	}
		
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		uiManager.registerApplicationUImanager(RootWebManager.getInstance());
		uiManager.registerApplicationUImanager(CompanyWebManager.getInstance());
	}
	
}
