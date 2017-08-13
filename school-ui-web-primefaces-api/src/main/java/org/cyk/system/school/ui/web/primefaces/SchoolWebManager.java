package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;

import javax.inject.Named;
import javax.inject.Singleton;

import org.cyk.system.company.model.structure.Company;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.root.model.party.person.JobTitle;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonTitle;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.Icon;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.api.model.AbstractTree;
import org.cyk.ui.web.api.AjaxListener.ListenValueMethod;
import org.cyk.ui.web.api.WebHierarchyNode;
import org.cyk.ui.web.primefaces.AbstractPrimefacesManager;
import org.cyk.ui.web.primefaces.HierarchyNode;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.UserSession;
import org.cyk.ui.web.primefaces.page.AbstractBusinessEntityFormOnePage;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;
import org.primefaces.model.TreeNode;

import lombok.Getter;

@Named @Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolWebManager.DEPLOYMENT_ORDER) @Getter
public class SchoolWebManager extends AbstractPrimefacesManager implements Serializable {

	public static final int DEPLOYMENT_ORDER = SchoolBusinessLayer.DEPLOYMENT_ORDER+1;
	private static final long serialVersionUID = 7231721191071228908L;

	private static SchoolWebManager INSTANCE;

	protected final String outcomeConfigureLevels = "configureLevels";
	//@Setter protected String academicSessionInfos="ACA INFOS TO SET";
	//@Setter protected String classroomSessionDivisionTypeName,classroomSessionDivisionInfos="CSD INFOS TO SET";
	protected String outcomeGenerateStudentClassroomSessionDivisionReport = "classroomSessionDivisionUpdateStudentReport";
	protected String outcomeUpdateStudentClassroomSessionDivisionResults  = "classroomSessionDivisionUpdateStudentResults";
	protected String outcomeConsultClassroomSessionStudentDivisionReportFile  = "classroomSessionConsultStudentDivisionReportFileView";
	protected String outcomeConsultSchoolStudentClassroomSessionDivisionReportFile  = "schoolConsultStudentClassroomSessionDivisionReportFileView";
	
	protected String outcomeEditStudentClassroomSessionDivisionEvaluationAverage  = "studentClassroomSessionDivisionEditEvaluationAverageView";
	protected String outcomeConsultClassroomSessionDivisionBroadsheet = "classroomSessionDivisionBroadsheetConsultView";
	protected String outcomeStudentClassroomSessionDivisionMergeReport  = "studentClassroomSessionDivisionMergeReportView";
	protected final String outcomeDefineTuition = "studentClassroomSessionTuitionEditView";
	protected final String outcomeStudentClassroomSessionConsultManyRank = "studentClassroomSessionConsultManyRankView";
	protected final String outcomeAssignSubjectClassroomSessionToStudentClassroomSession = "subjectClassroomSessionAssignToStudentClassroomSessionView";
	/*private String outcomeClassroomSessionMainDetails;
	private String outcomeClassroomSessionDivisionDetails;
	private String outcomeClassroomSessionSubjectDetails;
	private String outcomeClassroomSessionStudentDetails;*/
	
	public static Boolean EVALUATION_EDITABLE_BY_TEACHER_ONLY = Boolean.TRUE;
	public static Boolean APPRECIATION_EDITABLE_BY_COODINATOR_ONLY = Boolean.TRUE;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		identifier = "school";
		super.initialisation(); 
	}
	/*
	public void doMoreInitialisation(){
		AcademicSession academicSession = inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted();
		academicSessionInfos = UIManager.getInstance().getTimeBusiness().formatExistencePeriodFromTo(academicSession.getExistencePeriod());
		classroomSessionDivisionTypeName = academicSession.getNodeInformations().getClassroomSessionTimeDivisionType().getName();
		classroomSessionDivisionInfos = "No "+(academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
	}*/
	
	public String getAcademicSessionInfos(){
		return UIManager.getInstance().getTimeBusiness().formatPeriodFromTo(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted().getExistencePeriod());
	}
	
	public String getClassroomSessionDivisionTypeName(){
		return inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted().getNodeInformations().getClassroomSessionTimeDivisionType().getName();
	}
	
	public String getClassroomSessionDivisionInfos(){
		return "No "+(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted().getNodeInformations().getCurrentClassroomSessionDivisionIndex());
	}
	
	@Override
	public SystemMenu systemMenu(UserSession userSession) {
		SystemMenu systemMenu = new SystemMenu();
		
		addBusinessMenu(userSession,systemMenu,getSchoolCommandable(userSession, null));
		addBusinessMenu(userSession,systemMenu,getRegistrationCommandable(userSession, null));
		addBusinessMenu(userSession,systemMenu,getRegularActivitiesCommandable(userSession, null));		
		addBusinessMenu(userSession,systemMenu,getResultsCardCommandable(userSession, null));
		
		//addBusinessMenu(systemMenu,CompanyWebManager.getInstance().getCustomerCommandable(userSession, null));
		//addBusinessMenu(systemMenu,CompanyWebManager.getInstance().getSaleCommandable(userSession, null,null));
		
		systemMenu.getReferenceEntities().add(getControlPanelCommandable(userSession, null));
		
		//initialiseNavigatorTree(userSession);
		return systemMenu;
	}
	
	public void configureBroadSheetTable(Table<?> table){
		
	}
	/*
	@Override
	public void initialiseNavigatorTree(UserSession userSession) {
		//userSession.setNavigatorTree(getNavigator(TreeNode.class,HierarchyNode.class,LevelGroup.class,userSession));
	}*/
	
	@SuppressWarnings("unchecked")
	@Override
	protected <TYPE> Collection<TYPE> getNavigatorTreeNodeDatas(Class<TYPE> dataClass,UserSession userSession) {
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER)){
			return (Collection<TYPE>) inject(LevelGroupBusiness.class).findAll();
		}else{
			Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userSession.getUser());
			if(teacher==null)
				return null;
			
			return (Collection<TYPE>) inject(LevelGroupBusiness.class).findByAcademicSessionByTeacher(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted()
					, teacher);
		}
	}
	
	@Override
	protected AbstractTree<TreeNode, HierarchyNode> createNavigatorTree(final UserSession userSession) {
		AbstractTree<TreeNode, HierarchyNode> tree = super.createNavigatorTree(userSession);
		tree.getTreeListeners().add((org.cyk.ui.api.model.AbstractTree.Listener<TreeNode, HierarchyNode>) new  AbstractTree.Listener.Adapter.Default<TreeNode,HierarchyNode>(){
			private static final long serialVersionUID = 1L;
			
			@Override
			public String getRootNodeLabel(Class<?> dataClass) {
				return languageBusiness.findClassLabelText(ClassroomSession.class);
			}
			
			@Override
			public TreeNode createNode(HierarchyNode model, TreeNode parent) {
				model.setCollapsedIcon(Icon.THING_FOLDER_COLLAPSED);
				model.setExpandedIcon(Icon.THING_FOLDER_EXPANDED);
				if(model.getData() instanceof ClassroomSessionDivisionSubjectEvaluationType){
					ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = (ClassroomSessionDivisionSubjectEvaluationType) model.getData();
					if(inject(IntervalBusiness.class).isLowerEqualsToHigher(classroomSessionDivisionSubjectEvaluationType.getCountInterval()) &&
							classroomSessionDivisionSubjectEvaluationType.getCountInterval().getLow().getValue().equals(BigDecimal.ONE))
						model.setCollapsedIcon(Icon.THING_TABLE);
						model.setExpandedIcon(Icon.THING_TABLE);
						Collection<Evaluation> evaluations = inject(EvaluationBusiness.class).findByClassroomSessionDivisionSubjectEvaluationType((ClassroomSessionDivisionSubjectEvaluationType) model.getData());
						if(evaluations.isEmpty())
							;
						else
							model.getCss().addClass("treenodeevaluationexistclass");
				}
				return super.createNode(model, parent);
			}
			
			@Override
			public Collection<?> children(Object object) { 
				/*if(object instanceof LevelGroup){
					LevelGroup levelGroup = (LevelGroup) object;
					if(Boolean.TRUE.equals(userSession.getIsManager()))
						return inject(ClassroomSessionBusiness.class).findByAcademicSessionByLevelGroup(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted(), levelGroup);
					else{
						Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userSession.getUser());
						if(teacher!=null)
							return inject(ClassroomSessionBusiness.class).findByAcademicSessionByLevelGroupByTeacher(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted(), levelGroup,teacher);
					}
						
				}
				if(object instanceof ClassroomSession){
					ClassroomSession classroomSession = (ClassroomSession) object;
					
					ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(classroomSession
							, inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSession).getCurrentClassroomSessionDivisionIndex());
					
					if(Boolean.TRUE.equals(userSession.getIsManager()))
						return inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivision(classroomSessionDivision);
					else{
						Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userSession.getUser());
						if(teacher!=null)
							return inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionByTeacher(classroomSessionDivision, teacher);
					}}
				if(object instanceof ClassroomSessionDivisionSubject){
					ClassroomSessionDivisionSubject classroomSessionDivisionSubject = (ClassroomSessionDivisionSubject) object;
					return inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);			
				}*/
				return super.children(object);
			}
			
			@Override
			public Boolean isLeaf(TreeNode node) {
				/*Object object = nodeModel(node).getData();
				if(object instanceof ClassroomSession){
					ClassroomSession classroomSession = (ClassroomSession) object;
					ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(classroomSession
							, inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSession).getCurrentClassroomSessionDivisionIndex());
					return classroomSessionDivision.getNumberOfSubjects() == 0;
				}*/
				return super.isLeaf(node);
			}
			
			@Override
			public Object getRedirectionObject(TreeNode node) {
				Object object = ((WebHierarchyNode)node.getData()).getData();
				if(object instanceof ClassroomSessionDivisionSubjectEvaluationType){
					ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = (ClassroomSessionDivisionSubjectEvaluationType) object;
					if(inject(IntervalBusiness.class).isLowerEqualsToHigher(classroomSessionDivisionSubjectEvaluationType.getCountInterval()) &&
							classroomSessionDivisionSubjectEvaluationType.getCountInterval().getLow().getValue().equals(BigDecimal.ONE))
						if(classroomSessionDivisionSubjectEvaluationType.getNumberOfEvaluations()==0)
							return new Evaluation();
						else{
							return inject(EvaluationBusiness.class).findByClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType)
									.iterator().next();
						}
				}
				return super.getRedirectionObject(node);
			}
			
			@Override
			public Crud getRedirectionCrud(TreeNode node) {
				Object object = ((WebHierarchyNode)node.getData()).getData();
				if(object instanceof ClassroomSessionDivisionSubjectEvaluationType){
					ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = (ClassroomSessionDivisionSubjectEvaluationType) object;
					System.out.println(classroomSessionDivisionSubjectEvaluationType.getNumberOfEvaluations());
					if(inject(IntervalBusiness.class).isLowerEqualsToHigher(classroomSessionDivisionSubjectEvaluationType.getCountInterval()) &&
							classroomSessionDivisionSubjectEvaluationType.getCountInterval().getLow().getValue().equals(BigDecimal.ONE))
						if(classroomSessionDivisionSubjectEvaluationType.getNumberOfEvaluations()==0)
							return Crud.CREATE;
						else
							return Crud.READ;
				}
				return super.getRedirectionCrud(node);
			}
		});
		return tree;
	}
	
	/**/
	
	protected Boolean isConnectedUserInstanceOfTeacher(UserSession userSession){
		return isConnectedUserInstanceOfActor(userSession, inject(TeacherBusiness.class));
	}
	
	/**/
	/*
	public List<SelectItem> getSubjectSelectItems(){
		return webManager.getSelectItems(Subject.class);
	}
	public List<SelectItem> getTeacherSelectItems(){
		return webManager.getSelectItems(Teacher.class);
	}*/
	
	/**/
	
	public UICommandable getSchoolCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER)){
			module = Builder.create("school", null);
			module.addChild(Builder.createList(AcademicSession.class, null));
			module.addChild(Builder.createList(LevelTimeDivision.class, null));
			module.addChild(Builder.createList(ClassroomSession.class, null));
		}
		return module;
	}
	
	public UICommandable getRegistrationCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER)){
			module = Builder.create("command.actor.registration", Icon.PERSON);
			module.addChild(Builder.createList(Student.class, null));
			module.addChild(Builder.createList(Teacher.class, null));
			module.addChild(Builder.createList(Employee.class, null));
			
			module.addChild(Builder.createCreateMany(StudentClassroomSession.class, null));
			module.addChild(Builder.createCreateMany(StudentClassroomSessionDivisionSubject.class, null));
		}
		return module;
	}
	
	public UICommandable getRegularActivitiesCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = Builder.create("school.activities", null);
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER) || isConnectedUserInstanceOfTeacher(userSession)){
			module.addChild(Builder.createSelectOne(ClassroomSessionDivisionSubjectEvaluationType.class,SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation() ,null));
		}
		return module;
	}
	
	public UICommandable getResultsCardCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = Builder.create("school.results", null).setIdentifier(COMMANDABLE_IDENTIFIER_RESULTS);
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER) || isConnectedUserInstanceOfTeacher(userSession)){
			module.addChild(Builder.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults() ,null));
			//module.addChild(Builder.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionConsultClassroomSessionDivisionBroadsheet() ,null));
		}
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER)){
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionUpdateClassroomSessionDivisionBroadsheet() ,null));
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultClassroomSessionDivisionBroadsheet() ,null));
			
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionEditStudentClassroomSessionDivisionEvaluationAverage() ,null));
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults() ,null));
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionEvaluationResults() ,null));
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles() ,null));
			addChild(userSession,module,Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles() ,null));
			
			addChild(userSession,module, Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionRanks(),null)
					.setIdentifier(COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS));
			
		}
		/*
		if(userSession.hasRole(Role.MANAGER)){
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults() ,null));
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionAttendanceResults() ,null));
			module.addChild(menuManager.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionRankResults() ,null));
		}
		*/
		
		onBusinessMenuPopulateEnded(userSession, module);
		return module;
	}
	
	public UICommandable getControlPanelCommandable(AbstractUserSession<?,?> userSession,Collection<UICommandable> mobileCommandables){
		UICommandable module = null;
		if(userSession.hasRole(RootConstant.Code.Role.MANAGER)){
			module = Builder.create("commandable.school", null);
			module.addChild(Builder.createList(Company.class, null));
			module.addChild(Builder.createList(PersonTitle.class, null));
			module.addChild(Builder.createList(JobTitle.class, null));
		}
		return module;
	}
	
	/**/
	
	public void initialiseSelectClassroomSession(final AbstractBusinessEntityFormOnePage<?> page,final String classroomSessionFieldName,final String classroomSessionDivisionFieldName
			,final String classroomSessionDivisionSubjectFieldName,final String subjectEvaluationTypeFieldName) {
		String actionIdentifier = webManager.getRequestParameter(UniformResourceLocatorParameter.ACTION_IDENTIFIER);
		Collection<ClassroomSession> classroomSessions = null;
		AcademicSession academicSession = inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted();
		final Teacher teacher = page.getUserSession().getUserAccount().getUser() instanceof Person 
				? inject(TeacherBusiness.class).findByPerson((Person) page.getUserSession().getUserAccount().getUser()) : null;
		if(!Boolean.TRUE.equals(page.getUserSession().getIsManager()) && Boolean.TRUE.equals(EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
			if(teacher==null)
				;
			else{
				if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults().equals(actionIdentifier))
					classroomSessions = inject(ClassroomSessionBusiness.class).findByAcademicSessionByCoordinator(academicSession,teacher);
				else
					classroomSessions = inject(ClassroomSessionBusiness.class).findByAcademicSessionByTeacher(academicSession,teacher);
			}
		}else {
			classroomSessions = inject(ClassroomSessionBusiness.class).findByAcademicSession(academicSession);
		}
		//page.setChoices(classroomSessionFieldName, classroomSessions);
		
		selectClassroomSession(page, teacher, (ClassroomSession) page.setChoicesAndGetAutoSelected(classroomSessionFieldName, classroomSessions), classroomSessionFieldName, classroomSessionDivisionFieldName
			, classroomSessionDivisionSubjectFieldName, subjectEvaluationTypeFieldName);
		
		if(classroomSessionDivisionFieldName!=null){
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
		}
		
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
			if(!Boolean.TRUE.equals(page.getUserSession().getIsManager()) && Boolean.TRUE.equals(EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
				classroomSessionDivisions = teacher==null?null:inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByTeacher(classroomSession,teacher);
			}else{
				classroomSessionDivisions = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSession(classroomSession);
			}
			page.setChoices(classroomSessionDivisionFieldName, classroomSessionDivisions);
			CommonNodeInformations commonNodeInformations = null;//inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSession);
			page.getForm().getInputByFieldName(classroomSessionDivisionFieldName).setDisabled(commonNodeInformations.getCurrentClassroomSessionDivisionIndex()!=null);
			if(commonNodeInformations.getCurrentClassroomSessionDivisionIndex()!=null){
				for(ClassroomSessionDivision c : classroomSessionDivisions)
					if(c.getOrderNumber().equals(commonNodeInformations.getCurrentClassroomSessionDivisionIndex()) ){
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
			if(!Boolean.TRUE.equals(page.getUserSession().getIsManager()) && Boolean.TRUE.equals(EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
				classroomSessionDivisionSubjects = teacher==null?null:inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionByTeacher(classroomSessionDivision,teacher);
			}else{
				classroomSessionDivisionSubjects = inject(ClassroomSessionDivisionSubjectBusiness.class)
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
			page.setChoices(subjectEvaluationTypeFieldName, inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class)
					.findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
		}
	}
	
	public static SchoolWebManager getInstance() {
		return INSTANCE;
	}
	
	/**/
	
	public static final String COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS = "COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS";
	public static final String COMMANDABLE_IDENTIFIER_RESULTS = "COMMANDABLE_IDENTIFIER_RESULTS";
	
	/**/
	
	public static interface Listener extends AbstractPrimefacesManagerListener {
		
		/**/
		
		public static class Adapter extends AbstractPrimefacesManagerListener.Adapter implements Listener{
			private static final long serialVersionUID = 3034803382486669232L;
			
			
		}
	}
}
