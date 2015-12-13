package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.cyk.system.company.model.structure.Company;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.api.AjaxListener.ListenValueMethod;
import org.cyk.ui.web.primefaces.AbstractPrimefacesManager;
import org.cyk.ui.web.primefaces.page.AbstractBusinessEntityFormOnePage;
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
		
		SystemMenu systemMenu = new SystemMenu();
		UICommandable commandable;
		
		UICommandable group = uiProvider.createCommandable("fonctionnalities", null);		
		group.addChild(menuManager.crudMany(Company.class, null));
		group.addChild(menuManager.crudMany(Employee.class, null));
		group.addChild(menuManager.crudMany(Teacher.class, null));
		group.addChild(menuManager.crudMany(Student.class, null));
		group.addChild(menuManager.crudMany(ClassroomSession.class, null));
		group.addChild(menuManager.createMany(StudentClassroomSession.class, null));
		group.addChild(menuManager.createMany(StudentSubject.class, null));
		group.addChild(commandable = menuManager.createSelect(ClassroomSessionDivisionSubject.class,SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation() ,null));
		commandable.setLabel("Create evaluation");
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
	
	public void initialiseSelectClassroomSessionDivisionSubject(final AbstractBusinessEntityFormOnePage<?> page,final String classroomSessionFieldName,final String classroomSessionDivisionFieldName
			,final String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName) {
		page.setChoices(classroomSessionFieldName, SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findByAcademicSession(
				SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null)));
		
		page.createAjaxBuilder(classroomSessionFieldName).updatedFieldNames(classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName)
		.method(ClassroomSession.class,new ListenValueMethod<ClassroomSession>() {
			@Override
			public void execute(ClassroomSession value) {
				selectClassroomSession(page,value,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
			}
		}).build();
		
		page.createAjaxBuilder(classroomSessionDivisionFieldName).updatedFieldNames(classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName)
		.method(ClassroomSessionDivision.class,new ListenValueMethod<ClassroomSessionDivision>() {
			@Override
			public void execute(ClassroomSessionDivision value) {
				selectClassroomSessionDivision(page,value,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
			}
		}).build();
		
		if(subjectEvaluationTypeFieldName!=null)
			page.createAjaxBuilder(classroomSessionDivisionSubjectFieldName).updatedFieldNames(subjectEvaluationTypeFieldName)
			.method(ClassroomSessionDivisionSubject.class,new ListenValueMethod<ClassroomSessionDivisionSubject>() {
				@Override
				public void execute(ClassroomSessionDivisionSubject value) {
					selectClassroomSessionDivisionSubject(page,value,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
				}
			}).build();
	}
	
	public static void selectClassroomSession(AbstractBusinessEntityFormOnePage<?> page,ClassroomSession classroomSession,String classroomSessionFieldName,String classroomSessionDivisionFieldName
			,String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName){
		if(classroomSession==null)
			page.setChoices(classroomSessionDivisionFieldName,null);
		else
			page.setChoices(classroomSessionDivisionFieldName, SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSession(classroomSession));
		
		selectClassroomSessionDivision(page,null,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
	}
	public static void selectClassroomSessionDivision(AbstractBusinessEntityFormOnePage<?> page,ClassroomSessionDivision classroomSessionDivision,String classroomSessionFieldName,String classroomSessionDivisionFieldName
			,String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName){
		if(classroomSessionDivision==null){
			page.setChoices(classroomSessionDivisionSubjectFieldName, null);
		}else{
			page.setChoices(classroomSessionDivisionSubjectFieldName, SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness()
					.findByClassroomSessionDivision(classroomSessionDivision));
		}
		selectClassroomSessionDivisionSubject(page, null, classroomSessionFieldName, classroomSessionDivisionFieldName, classroomSessionDivisionSubjectFieldName, subjectEvaluationTypeFieldName);
	}
	public static void selectClassroomSessionDivisionSubject(AbstractBusinessEntityFormOnePage<?> page,ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String classroomSessionFieldName,String classroomSessionDivisionFieldName
			,String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName){
		if(classroomSessionDivisionSubject==null){
			page.setChoices(subjectEvaluationTypeFieldName, null);
		}else{
			page.setChoices(subjectEvaluationTypeFieldName, SchoolBusinessLayer.getInstance().getSubjectEvaluationTypeBusiness()
					.findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
		}
	}
	
	/**/
	
	public static SchoolWebManager getInstance() {
		return INSTANCE;
	}

	
}
