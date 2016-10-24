package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.security.Role;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.actor.TeacherBusinessImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelSpeciality;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.model.subject.SubjectGroup;
import org.cyk.ui.api.Icon;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.api.model.AbstractTree;
import org.cyk.ui.web.api.WebHierarchyNode;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.HierarchyNode;
import org.cyk.ui.web.primefaces.UserSession;
import org.primefaces.model.TreeNode;

public class SystemMenuBuilder extends org.cyk.system.company.ui.web.primefaces.adapter.enterpriseresourceplanning.SystemMenuBuilder implements Serializable {

	private static final long serialVersionUID = 6995162040038809581L;

	private static SystemMenuBuilder INSTANCE;
	
	@Override
	public SystemMenu build(UserSession userSession) {
		SystemMenu systemMenu = super.build(userSession);
		addBusinessMenu(userSession,systemMenu,getStudentCommandable(userSession, null));
		initialiseNavigatorTree(userSession);
		return systemMenu;
	}
	
	public Commandable getAcademicCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("command.academic.management", null);
		
		if(userSession.hasRole(Role.MANAGER) || userSession.isUserInstanceOf(Teacher.class)){
			module.addChild(Builder.createSelectOne(ClassroomSessionDivisionSubjectEvaluationType.class,SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation() ,null));
		}
		
		if(userSession.hasRole(Role.MANAGER) || userSession.isUserInstanceOf(Teacher.class)){
			module.addChild(Builder.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults() ,null));
			module.addChild(Builder.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionConsultClassroomSessionDivisionBroadsheet() ,null));
		}
		if(userSession.hasRole(Role.MANAGER)){
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionEditStudentClassroomSessionDivisionEvaluationAverage() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionEvaluationResults() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles() ,null));
			
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionRanks(),null)
					.setIdentifier(COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS));	
		}
		
		module.addChild(createListCommandable(AcademicSession.class, null));
		module.addChild(createListCommandable(LevelTimeDivision.class, null));
		module.addChild(createListCommandable(ClassroomSession.class, null));
		/*module.addChild(createListCommandable(ClassroomSessionDivision.class, null));
		module.addChild(createListCommandable(ClassroomSessionDivisionSubject.class, null));
		
		module.addChild(createListCommandable(Evaluation.class, null));
		module.addChild(createListCommandable(StudentClassroomSessionDivision.class, null));
		*/
		module.addChild(createListCommandable(Subject.class, null));
		return module;
	}
	
	public Commandable getStudentCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("command.student.management", null);
		module.addChild(createListCommandable(Student.class, null));
		module.addChild(createListCommandable(StudentClassroomSession.class, null));
		module.addChild(Builder.createCreateMany(StudentClassroomSession.class, null));
		module.addChild(Builder.createCreateMany(StudentClassroomSessionDivisionSubject.class, null));
		addReportCommandables(Student.class,module, StudentBusinessImpl.Listener.COLLECTION);
		return module;
	}
	
	public Commandable getLevelCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable(Level.class, null);
		module.addChild(createListCommandable(LevelName.class, null));
		module.addChild(createListCommandable(LevelSpeciality.class, null));
		return module;
	}
	
	@Override
	public Commandable getEmployeeCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables) {
		Commandable module = super.getEmployeeCommandable(userSession, mobileCommandables);
		module.addChild(createListCommandable(Teacher.class, null));
		addReportCommandables(Teacher.class,module, TeacherBusinessImpl.Listener.COLLECTION);
		return module;
	}
	
	/**/
	
	public Commandable getRegularActivitiesCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable("school.activities", null);
		if(userSession.hasRole(Role.MANAGER) || userSession.isUserInstanceOf(Teacher.class)){
			module.addChild(Builder.createSelectOne(ClassroomSessionDivisionSubjectEvaluationType.class,SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation() ,null));
		}
		return module;
	}
	
	public Commandable getResultsCardCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = (Commandable) createModuleCommandable("school.results", null).setIdentifier(COMMANDABLE_IDENTIFIER_RESULTS);
		if(userSession.hasRole(Role.MANAGER) || userSession.isUserInstanceOf(Teacher.class)){
			module.addChild(Builder.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults() ,null));
			module.addChild(Builder.createSelectOne(ClassroomSessionDivision.class,SchoolBusinessLayer.getInstance().getActionConsultClassroomSessionDivisionBroadsheet() ,null));
		}
		if(userSession.hasRole(Role.MANAGER)){
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionEditStudentClassroomSessionDivisionEvaluationAverage() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionDivisionEvaluationResults() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionComputeStudentClassroomSessionEvaluationResults() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles() ,null));
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles() ,null));
			
			module.addChild(Builder.createSelectMany(ClassroomSession.class,SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionRanks(),null)
					.setIdentifier(COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS));	
		}
		
		return module;
	}
	
	/**/
	
	@Override
	protected void addReferences(UserSession userSession,SystemMenu systemMenu, Collection<UICommandable> mobileCommandables) {
		addReference(userSession, systemMenu, getReferenceSchoolCommandable(userSession, mobileCommandables));
		addReference(userSession, systemMenu, getReferenceSessionCommandable(userSession, mobileCommandables));
		super.addReferences(userSession, systemMenu, mobileCommandables);
	}
	
	public Commandable getReferenceSchoolCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable(UIManager.getInstance().businessEntityInfos(School.class).getUserInterface().getLabelId(), null);
		module.addChild(createListCommandable(School.class, null));
		module.addChild(createListCommandable(Subject.class, null));
		module.addChild(createListCommandable(SubjectGroup.class, null));
		module.addChild(createListCommandable(LevelGroupType.class, null));
		module.addChild(createListCommandable(LevelGroup.class, null));
		module.addChild(createListCommandable(LevelName.class, null));
		module.addChild(createListCommandable(LevelSpeciality.class, null));
		module.addChild(createListCommandable(Level.class, null));
		module.addChild(createListCommandable(EvaluationType.class, null));
		return module;
	}
	
	public Commandable getReferenceSessionCommandable(UserSession userSession,Collection<UICommandable> mobileCommandables){
		Commandable module = createModuleCommandable(UIManager.getInstance().businessEntityInfos(AcademicSession.class).getUserInterface().getLabelId(), null);
		module.addChild(createListCommandable(AcademicSession.class, null));
		module.addChild(createListCommandable(LevelTimeDivision.class, null));
		return module;
	}
	
	/**/
	
	@Override
	protected void initialiseNavigatorTree(UserSession userSession) {
		super.initialiseNavigatorTree(userSession);
		userSession.setNavigatorTree(getNavigator(TreeNode.class,HierarchyNode.class,LevelGroup.class,userSession));
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected <TYPE> Collection<TYPE> getNavigatorTreeNodeDatas(Class<TYPE> dataClass,UserSession userSession) {
		if(userSession.hasRole(Role.MANAGER)){
			return (Collection<TYPE>) inject(LevelGroupBusiness.class).findAll();
		}else{
			Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userSession.getUser());
			if(teacher==null)
				return null;
			
			return (Collection<TYPE>) inject(LevelGroupBusiness.class).findByAcademicSessionByTeacher(inject(AcademicSessionBusiness.class).findCurrent(null)
					, teacher);
		}
	}
	
	@Override
	protected AbstractTree<TreeNode, HierarchyNode> createNavigatorTree(final UserSession userSession) {
		AbstractTree<TreeNode, HierarchyNode> tree = /*new Tree();*/super.createNavigatorTree(userSession);
		tree.getTreeListeners().add((org.cyk.ui.api.model.AbstractTree.Listener<TreeNode, HierarchyNode>) new  AbstractTree.Listener.Adapter.Default<TreeNode,HierarchyNode>(){
			private static final long serialVersionUID = 1L;
			
			@Override
			public String getRootNodeLabel(Class<?> dataClass) {
				return inject(LanguageBusiness.class).findClassLabelText(ClassroomSession.class);
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
				if(object instanceof LevelGroup){
					LevelGroup levelGroup = (LevelGroup) object;
					if(Boolean.TRUE.equals(userSession.getIsManager()))
						return inject(ClassroomSessionBusiness.class).findByAcademicSessionByLevelGroup(inject(AcademicSessionBusiness.class).findCurrent(null), levelGroup);
					else{
						Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userSession.getUser());
						if(teacher!=null)
							return inject(ClassroomSessionBusiness.class).findByAcademicSessionByLevelGroupByTeacher(inject(AcademicSessionBusiness.class).findCurrent(null), levelGroup,teacher);
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
				}
				return super.children(object);
			}
			
			@Override
			public Boolean isLeaf(TreeNode node) {
				Object object = nodeModel(node).getData();
				if(object instanceof ClassroomSession){
					ClassroomSession classroomSession = (ClassroomSession) object;
					ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(classroomSession
							, inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSession).getCurrentClassroomSessionDivisionIndex());
					return classroomSessionDivision.getNumberOfSubjects() == 0;
				}
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
	
	public static SystemMenuBuilder getInstance(){
		if(INSTANCE==null)
			INSTANCE = new SystemMenuBuilder();
		return INSTANCE;
	}

	/**/
	
	public static final String COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS = "COMMANDABLE_IDENTIFIER_CONSULT_STUDENTCLASSROOMSESSION_RANKS";
	public static final String COMMANDABLE_IDENTIFIER_RESULTS = "COMMANDABLE_IDENTIFIER_RESULTS";
}
