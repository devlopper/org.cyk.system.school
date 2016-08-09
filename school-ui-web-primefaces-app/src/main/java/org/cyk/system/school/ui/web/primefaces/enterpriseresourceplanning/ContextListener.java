package org.cyk.system.school.ui.web.primefaces.enterpriseresourceplanning;

import java.io.Serializable;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

import org.cyk.system.root.ui.web.primefaces.api.RootWebManager;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.AbstractContextListener;
import org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager;

//@WebListener
public class ContextListener extends AbstractContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		uiManager.registerApplicationUImanager(RootWebManager.getInstance());
		SchoolWebManager.getInstance().getListeners().add(new PrimefacesManager());
	}
	
}
