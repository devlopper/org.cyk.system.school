package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.AbstractPrimefacesManager;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

import lombok.Getter;
import lombok.Setter;

@Named @Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolWebManager.DEPLOYMENT_ORDER) @Getter
public class SchoolWebManager extends AbstractPrimefacesManager implements Serializable {

	public static final int DEPLOYMENT_ORDER = SchoolBusinessLayer.DEPLOYMENT_ORDER+1;
	private static final long serialVersionUID = 7231721191071228908L;

	private static SchoolWebManager INSTANCE;
	
	private final String outcomeConfigureLevels = "configureLevels";
	@Getter @Setter private String academicSessionInfos="INFOS TO SET";
	private String outcomeGenerateStudentClassroomSessionDivisionReport = "classroomSessionDivisionUpdateStudentReport";
	/*private String outcomeClassroomSessionMainDetails;
	private String outcomeClassroomSessionDivisionDetails;
	private String outcomeClassroomSessionSubjectDetails;
	private String outcomeClassroomSessionStudentDetails;*/
	
	@Inject private AcademicSessionBusiness academicSessionBusiness;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation(); 
		identifier = "school";
		academicSessionInfos = UIManager.getInstance().getTimeBusiness().formatPeriodFromTo(academicSessionBusiness.findCurrent(null).getPeriod());
	}
		
	@Override
	public SystemMenu systemMenu(AbstractUserSession userSession) {
		businessEntityInfos(LevelName.class).setUiEditViewId("editLevelName");
		
		SystemMenu systemMenu = new SystemMenu();
		
		
		UICommandable group = uiProvider.createCommandable("fonctionnalities", null);		
		group.addChild(menuManager.crudMany(Teacher.class, null));
		group.addChild(menuManager.crudMany(Student.class, null));
		group.addChild(menuManager.crudMany(ClassroomSession.class, null));
		//group.addChild(uiProvider.createCommandable("dashboard", null, outcomeGenerateStudentClassroomSessionDivisionReport));
		
		/*
		group.addChild(menuManager.crudMany(Student.class, null));
		group.addChild(menuManager.crudMany(Teacher.class, null));
		group.addChild(menuManager.crudMany(Teacher.class, null));
		group.addChild(menuManager.crudMany(Teacher.class, null));
		
		group.addChild(uiProvider.createCommandable("dashboard", null, outcomeSaleDashBoard));
		*/
		
		systemMenu.getBusinesses().add(group);
					
		return systemMenu;
	}
	
	/**/
	
	public static SchoolWebManager getInstance() {
		return INSTANCE;
	}

	
}
