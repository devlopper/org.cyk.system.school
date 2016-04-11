package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.util.Collection;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.model.structure.Company;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonTitle;
import org.cyk.system.root.model.security.Role;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.UICommandable.IconType;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.api.model.AbstractTree;
import org.cyk.ui.api.model.HierarchyNode;
import org.cyk.ui.web.api.AjaxListener.ListenValueMethod;
import org.cyk.ui.web.api.WebHierarchyNode;
import org.cyk.ui.web.primefaces.AbstractPrimefacesManager;
import org.cyk.ui.web.primefaces.Tree;
import org.cyk.ui.web.primefaces.page.AbstractBusinessEntityFormOnePage;

@Getter
public abstract class AbstractSchoolWebManager extends AbstractPrimefacesManager implements Serializable {

	private static final long serialVersionUID = 7231721191071228908L;

	protected final String outcomeConfigureLevels = "configureLevels";
	@Setter protected String academicSessionInfos="ACA INFOS TO SET";
	@Setter protected String classroomSessionDivisionTypeName,classroomSessionDivisionInfos="CSD INFOS TO SET";
	protected String outcomeGenerateStudentClassroomSessionDivisionReport = "classroomSessionDivisionUpdateStudentReport";
	protected String outcomeUpdateStudentClassroomSessionDivisionResults  = "classroomSessionDivisionUpdateStudentResults";
	protected String outcomeConsultClassroomSessionStudentDivisionReportFile  = "classroomSessionConsultStudentDivisionReportFileView";
	protected String outcomeConsultSchoolStudentClassroomSessionDivisionReportFile  = "schoolConsultStudentClassroomSessionDivisionReportFileView";
	
	protected String outcomeStudentClassroomSessionDivisionMergeReport  = "studentClassroomSessionDivisionMergeReportView";
	/*private String outcomeClassroomSessionMainDetails;
	private String outcomeClassroomSessionDivisionDetails;
	private String outcomeClassroomSessionSubjectDetails;
	private String outcomeClassroomSessionStudentDetails;*/
	
	public static Boolean EVALUATION_EDITABLE_BY_TEACHER_ONLY = Boolean.TRUE;
	public static Boolean APPRECIATION_EDITABLE_BY_COODINATOR_ONLY = Boolean.TRUE;
	
	@Override
	protected void initialisation() {
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
		
		addBusinessMenu(systemMenu,getRegistrationCommandable(userSession, null));
		addBusinessMenu(systemMenu,getClassCommandable(userSession, null));			
		//addBusinessMenu(systemMenu,getResultsCardCommandable(userSession, null));
		addBusinessMenu(systemMenu,getMarksCardCommandable(userSession, null));
		
		systemMenu.getReferenceEntities().add(getControlPanelCommandable(userSession, null));
		userSession.setNavigator(getNavigator(org.primefaces.component.tree.Tree.class,WebHierarchyNode.class,userSession));
		return systemMenu;
	}
	
	/**/
	
	public UICommandable getRegistrationCommandable(AbstractUserSession<?,?> userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(Role.MANAGER)){
			module = uiProvider.createCommandable("command.actor.registration", IconType.PERSON);
			module.addChild(menuManager.crudMany(Student.class, null));
			module.addChild(menuManager.crudMany(Teacher.class, null));
			module.addChild(menuManager.crudMany(Employee.class, null));
		}
		return module;
	}
	
	public UICommandable getClassCommandable(AbstractUserSession<?,?> userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = uiProvider.createCommandable(businessEntityInfos(ClassroomSession.class).getUserInterface().getLabelId(), null);
		if(userSession.hasRole(Role.MANAGER)){
			module.addChild(menuManager.crudMany(ClassroomSession.class, null));
			module.addChild(menuManager.createMany(StudentClassroomSession.class, null));
			module.addChild(menuManager.createMany(StudentSubject.class, null));
		}
		if(userSession.hasRole(Role.MANAGER) || SchoolBusinessLayer.getInstance().getTeacherBusiness().findByPerson((Person) userSession.getUser())!=null){
			module.addChild(menuManager.createSelectOne(ClassroomSessionDivisionSubjectEvaluationType.class,SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation() ,null));
			module.addChild(menuManager.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults() ,null));
		}
		return module;
	}
	
	public UICommandable getResultsCardCommandable(AbstractUserSession<?,?> userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(Role.MANAGER)){
			module = uiProvider.createCommandable("school.results", null);
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults() ,null));
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionAttendanceResults() ,null));
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionRankResults() ,null));
		}
		return module;
	}
	
	public UICommandable getMarksCardCommandable(AbstractUserSession<?,?> userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(Role.MANAGER)){
			module = uiProvider.createCommandable("school.markscard", null);
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles() ,null));
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles() ,null));
		}
		return module;
	}
	
	public UICommandable getControlPanelCommandable(AbstractUserSession<?,?> userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(Role.MANAGER)){
			module = uiProvider.createCommandable("commandable.school", null);
			module.addChild(menuManager.crudMany(Company.class, null));
			module.addChild(menuManager.crudMany(PersonTitle.class, null));
			module.addChild(menuManager.crudMany(JobTitle.class, null));
		}
		return module;
	}
	
	@Override
	protected <NODE, NODE_MODEL extends HierarchyNode> AbstractTree<NODE, NODE_MODEL> createNavigatorTree(AbstractUserSession<NODE, NODE_MODEL> userSession) {
		AbstractTree<NODE, NODE_MODEL> tree = super.createNavigatorTree(userSession);
		//((Tree)tree).setOutcome(uiManager.businessEntityInfos(ClassroomSession.class).getUserInterface().getConsultViewId());
		//tree.getTreeListeners().add(new  TreeListener<NODE, NODE_MODEL>() {});
		return tree;
	}
	
	@Override
	protected <NODE, NODE_MODEL extends HierarchyNode> Collection<?> getNavigatorTreeNodeDatas(AbstractUserSession<NODE, NODE_MODEL> userSession) {
		if(userSession.hasRole(Role.MANAGER)){
			return SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll();
		}else{
			return null;
		}
	}
	
	/**/
	
	public void initialiseSelectClassroomSession(final AbstractBusinessEntityFormOnePage<?> page,final String classroomSessionFieldName,final String classroomSessionDivisionFieldName
			,final String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName) {
		
		Collection<ClassroomSession> classroomSessions = null;
		AcademicSession academicSession = SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null);
		final Teacher teacher = page.getUserSession().getUserAccount().getUser() instanceof Person 
				? SchoolBusinessLayer.getInstance().getTeacherBusiness().findByPerson((Person) page.getUserSession().getUserAccount().getUser()) : null;
		
		if(!Boolean.TRUE.equals(page.getUserSession().getIsAdministrator()) && Boolean.TRUE.equals(EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
			classroomSessions = teacher==null?null:SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findByAcademicSessionByTeacher(academicSession,teacher);
		}else {
			classroomSessions = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findByAcademicSession(academicSession);
		}
		
		//page.setChoices(classroomSessionFieldName, classroomSessions);
		
		selectClassroomSession(page, teacher, (ClassroomSession) page.setChoicesAndGetAutoSelected(classroomSessionFieldName, classroomSessions), classroomSessionFieldName, classroomSessionDivisionFieldName
			, classroomSessionDivisionSubjectFieldName, subjectEvaluationTypeFieldName);
		
		page.createAjaxBuilder(classroomSessionFieldName).updatedFieldNames(classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName)
		.method(ClassroomSession.class,new ListenValueMethod<ClassroomSession>() {
			@Override
			public void execute(ClassroomSession value) {
				selectClassroomSession(page,teacher,value,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
			}
		}).build();
		
		page.createAjaxBuilder(classroomSessionDivisionFieldName).updatedFieldNames(classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName)
		.method(ClassroomSessionDivision.class,new ListenValueMethod<ClassroomSessionDivision>() {
			@Override
			public void execute(ClassroomSessionDivision value) {
				selectClassroomSessionDivision(page,teacher,value,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
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
	
	public static void selectClassroomSession(AbstractBusinessEntityFormOnePage<?> page,Teacher teacher,ClassroomSession classroomSession,String classroomSessionFieldName,String classroomSessionDivisionFieldName
			,String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName){
		ClassroomSessionDivision classroomSessionDivision = null;
		if(classroomSession==null)
			page.setChoices(classroomSessionDivisionFieldName,null);
		else{
			Collection<ClassroomSessionDivision> classroomSessionDivisions;
			if(!Boolean.TRUE.equals(page.getUserSession().getIsAdministrator()) && Boolean.TRUE.equals(EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
				classroomSessionDivisions = teacher==null?null:SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSessionByTeacher(classroomSession,teacher);
			}else{
				classroomSessionDivisions = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSession(classroomSession);
			}
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
		selectClassroomSessionDivision(page,teacher,classroomSessionDivision,classroomSessionFieldName,classroomSessionDivisionFieldName,classroomSessionDivisionSubjectFieldName,subjectEvaluationTypeFieldName);
	}
	
	public static void selectClassroomSessionDivision(AbstractBusinessEntityFormOnePage<?> page,Teacher teacher,ClassroomSessionDivision classroomSessionDivision,String classroomSessionFieldName,String classroomSessionDivisionFieldName
			,String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName){
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = null;
		if(classroomSessionDivision==null){
			page.setChoices(classroomSessionDivisionSubjectFieldName, null);
		}else{
			Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects;
			if(!Boolean.TRUE.equals(page.getUserSession().getIsAdministrator()) && Boolean.TRUE.equals(EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
				classroomSessionDivisionSubjects = teacher==null?null:SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findByClassroomSessionDivisionByTeacher(classroomSessionDivision,teacher);
			}else{
				classroomSessionDivisionSubjects = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness()
						.findByClassroomSessionDivision(classroomSessionDivision);
			}
			//page.setChoices(classroomSessionDivisionSubjectFieldName, classroomSessionDivisionSubjects,classroomSessionDivision);
			classroomSessionDivisionSubject = (ClassroomSessionDivisionSubject) page.setChoicesAndGetAutoSelected(classroomSessionDivisionSubjectFieldName, classroomSessionDivisionSubjects);
			page.setFieldValue(classroomSessionDivisionFieldName, classroomSessionDivision);
		}
		selectClassroomSessionDivisionSubject(page, classroomSessionDivisionSubject, classroomSessionFieldName, classroomSessionDivisionFieldName, classroomSessionDivisionSubjectFieldName, subjectEvaluationTypeFieldName);
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
	
}
