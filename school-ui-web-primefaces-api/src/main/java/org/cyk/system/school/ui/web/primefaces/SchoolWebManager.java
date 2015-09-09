package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Singleton;

import lombok.Getter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelSpeciality;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.EvaluationTypeName;
import org.cyk.system.school.model.subject.SubjectGroupName;
import org.cyk.system.school.model.subject.SubjectName;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.AbstractPrimefacesManager;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolWebManager.DEPLOYMENT_ORDER) @Getter
public class SchoolWebManager extends AbstractPrimefacesManager implements Serializable {

	public static final int DEPLOYMENT_ORDER = SchoolBusinessLayer.DEPLOYMENT_ORDER+1;
	private static final long serialVersionUID = 7231721191071228908L;

	private static SchoolWebManager INSTANCE;
	
	private final String outcomeConfigureLevels = "configureLevels";
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation(); 
		identifier = "school";
	}
		
	@Override
	public SystemMenu systemMenu(AbstractUserSession userSession) {
		businessEntityInfos(LevelName.class).setUiEditViewId("editLevelName");
		
		SystemMenu systemMenu = new SystemMenu();
		
		
		UICommandable group = uiProvider.createCommandable("level", null);
		//group.addChild("level", null, outcomeConfigureLevels, null);
		
		group.addChild(menuManager.crudMany(LevelName.class, null));	
		group.addChild(menuManager.crudMany(LevelSpeciality.class, null));
		group.addChild(menuManager.crudMany(Level.class, null));
		group.addChild(menuManager.crudMany(LevelTimeDivision.class, null));
		group.addChild(menuManager.crudMany(EvaluationTypeName.class, null));
		group.addChild(menuManager.crudMany(SubjectName.class, null));
		group.addChild(menuManager.crudMany(SubjectGroupName.class, null));
		systemMenu.getReferenceEntities().add(group);
		
		
		/**/
		/*
		if(userSession.hasRole(CompanyBusinessLayer.getInstance().getRoleHumanResourcesManagerCode())){
			UICommandable hr = uiProvider.createCommandable("command.humanresources", IconType.PERSON);
			hr.addChild(c = menuManager.crudMany(Employee.class, null));
			//hr.addChild(c = menuManager.crudOne(Customer.class, null));
			//c.setLabel(uiManager.text("command.customer.new"));
			hr.addChild(c = menuManager.crudMany(Customer.class, null));
			hr.addChild(c = menuManager.crudMany(Cashier.class, null));
			systemMenu.getBusinesses().add(hr);
		}
		*/
			
		return systemMenu;
	}
	
	/**/
	
	public static SchoolWebManager getInstance() {
		return INSTANCE;
	}

	
}
