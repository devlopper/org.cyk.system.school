package org.cyk.system.school.ui.web.primefaces.enterpriseresourceplanning;

import java.io.Serializable;

import javax.servlet.ServletContextEvent;

import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.AbstractContextListener;
import org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager;

@javax.servlet.annotation.WebListener
public class ContextListener extends AbstractContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		SchoolWebManager.getInstance().getListeners().add(new PrimefacesManager());
		
		inject(RootBusinessLayer.class).enableEnterpriseResourcePlanning();
		inject(CompanyBusinessLayer.class).enableEnterpriseResourcePlanning();
		inject(SchoolBusinessLayer.class).enableEnterpriseResourcePlanning();
	}
		
}
