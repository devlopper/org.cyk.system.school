package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.model.structure.Company;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.PersonTitle;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.UICommandable.IconType;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.api.AjaxListener.ListenValueMethod;
import org.cyk.ui.web.primefaces.AbstractPrimefacesManager;
import org.cyk.ui.web.primefaces.page.AbstractBusinessEntityFormOnePage;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Named @Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolWebManager.DEPLOYMENT_ORDER) @Getter
public class SchoolWebManager extends AbstractPrimefacesManager implements Serializable {

	public static final int DEPLOYMENT_ORDER = SchoolBusinessLayer.DEPLOYMENT_ORDER+1;
	private static final long serialVersionUID = 7231721191071228908L;

	private static SchoolWebManager INSTANCE;
	
	private final String outcomeConfigureLevels = "configureLevels";
	@Setter private String academicSessionInfos="ACA INFOS TO SET";
	@Setter private String classroomSessionDivisionTypeName,classroomSessionDivisionInfos="CSD INFOS TO SET";
	private String outcomeGenerateStudentClassroomSessionDivisionReport = "classroomSessionDivisionUpdateStudentReport";
	private String outcomeUpdateStudentClassroomSessionDivisionResults  = "classroomSessionDivisionUpdateStudentResults";
	private String outcomeStudentClassroomSessionDivisionMergeReport  = "studentClassroomSessionDivisionMergeReportView";
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
		AcademicSession academicSession = SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null);
		academicSessionInfos = UIManager.getInstance().getTimeBusiness().formatPeriodFromTo(academicSession.getPeriod());
		classroomSessionDivisionTypeName = academicSession.getNodeInformations().getClassroomSessionTimeDivisionType().getName();
		classroomSessionDivisionInfos = "No "+academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex();
	}
	
	@Override
	public SystemMenu systemMenu(AbstractUserSession userSession) {
		
		SystemMenu systemMenu = new SystemMenu();
		
		systemMenu.getBusinesses().add(getRegistrationCommandable(userSession, null));
		systemMenu.getBusinesses().add(getClassCommandable(userSession, null));			
		
		systemMenu.getReferenceEntities().add(getControlPanelCommandable(userSession, null));
		
		return systemMenu;
	}
	
	/**/
	
	public UICommandable getRegistrationCommandable(AbstractUserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = uiProvider.createCommandable("command.actor.registration", IconType.PERSON);
		module.addChild(menuManager.crudMany(Student.class, null));
		module.addChild(menuManager.crudMany(Teacher.class, null));
		module.addChild(menuManager.crudMany(Employee.class, null));
		return module;
	}
	
	public UICommandable getClassCommandable(AbstractUserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = uiProvider.createCommandable(businessEntityInfos(ClassroomSession.class).getUserInterface().getLabelId(), null);
		module.addChild(menuManager.crudMany(ClassroomSession.class, null));
		module.addChild(menuManager.createMany(StudentClassroomSession.class, null));
		module.addChild(menuManager.createMany(StudentSubject.class, null));
		module.addChild(menuManager.createSelectOne(ClassroomSessionDivisionSubjectEvaluationType.class,SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation() ,null));
		module.addChild(menuManager.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults() ,null));
		module.addChild(menuManager.createSelectMany(StudentClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles() ,null));
		module.addChild(uiProvider.createCommandable("command.school.downloadmarkscard", IconType.ACTION_DOWNLOAD,outcomeStudentClassroomSessionDivisionMergeReport));
		return module;
	}
	
	public UICommandable getControlPanelCommandable(AbstractUserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = uiProvider.createCommandable("commandable.school", null);
		module.addChild(menuManager.crudMany(Company.class, null));
		module.addChild(menuManager.crudMany(PersonTitle.class, null));
		module.addChild(menuManager.crudMany(JobTitle.class, null));
		return module;
	}
	
	/**/
	
	public void initialiseSelectClassroomSession(final AbstractBusinessEntityFormOnePage<?> page,final String classroomSessionFieldName,final String classroomSessionDivisionFieldName
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
		ClassroomSessionDivision classroomSessionDivision = null;
		if(classroomSession==null)
			page.setChoices(classroomSessionDivisionFieldName,null);
		else{
			Collection<ClassroomSessionDivision> classroomSessionDivisions = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSession(classroomSession);
			page.setChoices(classroomSessionDivisionFieldName, classroomSessionDivisions);
			CommonNodeInformations commonNodeInformations = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findCommonNodeInformations(classroomSession);
			page.getForm().findInputByFieldName(classroomSessionDivisionFieldName).setDisabled(commonNodeInformations.getCurrentClassroomSessionDivisionIndex()!=null);
			if(commonNodeInformations.getCurrentClassroomSessionDivisionIndex()!=null){
				for(ClassroomSessionDivision c : classroomSessionDivisions)
					if(c.getIndex().equals(commonNodeInformations.getCurrentClassroomSessionDivisionIndex()) ){
						classroomSessionDivision = c;
						break;
					}
			}
		}
		selectClassroomSessionDivision(page,classroomSessionDivision,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
	}
	
	public static void selectClassroomSessionDivision(AbstractBusinessEntityFormOnePage<?> page,ClassroomSessionDivision classroomSessionDivision,String classroomSessionFieldName,String classroomSessionDivisionFieldName
			,String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName){
		if(classroomSessionDivision==null){
			page.setChoices(classroomSessionDivisionSubjectFieldName, null);
		}else{
			page.setChoices(classroomSessionDivisionSubjectFieldName, SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness()
					.findByClassroomSessionDivision(classroomSessionDivision),classroomSessionDivision);
			page.setFieldValue(classroomSessionDivisionFieldName, classroomSessionDivision);
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
	
	
	/**/
	
	public static SchoolWebManager getInstance() {
		return INSTANCE;
	}

	
}
