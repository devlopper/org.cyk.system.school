package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;

import org.cyk.system.company.ui.web.primefaces.AbstractCompanyContextListener;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.business.impl.session.AcademicSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.LevelTimeDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.SubjectClassroomSessionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectEvaluationTypeDetails;
import org.cyk.system.school.business.impl.subject.EvaluationDetails;
import org.cyk.system.school.business.impl.subject.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.system.school.ui.web.primefaces.session.AcademicSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionQueryOneFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEvaluationTypeEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionQueryManyFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionQueryOneFormModel;
import org.cyk.system.school.ui.web.primefaces.session.LevelTimeDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.SubjectClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionDivisionQueryManyFormModel;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentQueryOneFormModel;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentSubjectEditPage;
import org.cyk.ui.api.config.IdentifiableConfiguration;
import org.cyk.ui.web.api.AbstractWebPage;
import org.cyk.ui.web.primefaces.page.AbstractProcessManyPage;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;

public abstract class AbstractSchoolContextListener extends AbstractCompanyContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	@Inject protected SchoolWebManager schoolWebManager;
		
	@Override
	protected void initialisation() {
		super.initialisation();
		AbstractWebPage.DEFAULT_LAYOUT.setWest("/org.cyk.ui.web.primefaces.school/include/layout/westtop.xhtml");
		uiManager.registerApplicationUImanager(SchoolWebManager.getInstance());	
	}
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		//schoolWebManager.doMoreInitialisation();
	}
	
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		//uiManager.businessEntityInfos(ClassroomSession.class).setUiConsultViewId("");
		//IdentifiableConfiguration identifiableConfiguration = uiManager.findConfiguration(ClassroomSession.class);
		//identifiableConfiguration.setForms(ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(Student.class, StudentEditPage.Form.class, StudentDetails.class,StudentQueryOneFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(Student.class, null);
		AbstractSelectOnePage.Listener.COLLECTION.add(new StudentQueryOneFormModel.PageAdapter());
		webNavigationManager.useDynamicSelectView(Student.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(AcademicSession.class, AcademicSessionEditPage.Form.class, AcademicSessionDetails.class,null,null,null));
		uiManager.configBusinessIdentifiable(AcademicSession.class, null);
		//webNavigationManager.useDynamicSelectView(AcademicSession.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(LevelTimeDivision.class, LevelTimeDivisionEditPage.Form.class, LevelTimeDivisionDetails.class,null,null,null));
		uiManager.configBusinessIdentifiable(LevelTimeDivision.class, null);
		//webNavigationManager.useDynamicSelectView(LevelTimeDivision.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSession.class, ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class,ClassroomSessionQueryOneFormModel.class,null,ClassroomSessionQueryManyFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSession.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSession.class);
		AbstractSelectOnePage.Listener.COLLECTION.add(new ClassroomSessionQueryOneFormModel.PageAdapter());
		AbstractSelectManyPage.Listener.COLLECTION.add(new ClassroomSessionQueryManyFormModel.PageAdapter());
		AbstractProcessManyPage.Listener.COLLECTION.add(new ClassroomSessionQueryManyFormModel.ProcessPageAdapter());
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(SubjectClassroomSession.class, SubjectClassroomSessionEditPage.Form.class, SubjectClassroomSessionDetails.class,null,null,null));
		uiManager.configBusinessIdentifiable(SubjectClassroomSession.class, null);
		webNavigationManager.useDynamicSelectView(SubjectClassroomSession.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivision.class, ClassroomSessionDivisionEditPage.Form.class, ClassroomSessionDivisionDetails.class
				,ClassroomSessionDivisionQueryOneFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivision.class);
		AbstractSelectOnePage.Listener.COLLECTION.add(new ClassroomSessionDivisionQueryOneFormModel.PageAdapter());
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectEditPage.Form.class, ClassroomSessionDivisionSubjectDetails.class
				,ClassroomSessionDivisionSubjectQueryFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubject.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubject.class);
		AbstractSelectOnePage.Listener.COLLECTION.add(new ClassroomSessionDivisionSubjectQueryFormModel.PageAdapter());
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubjectEvaluationType.class, ClassroomSessionDivisionSubjectEvaluationTypeEditPage.Form.class, ClassroomSessionDivisionSubjectEvaluationTypeDetails.class
				,ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubjectEvaluationType.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubjectEvaluationType.class);
		AbstractSelectOnePage.Listener.COLLECTION.add(new ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.PageAdapter());
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(StudentClassroomSession.class, StudentClassroomSessionEditPage.Form.class, StudentClassroomSessionDetails.class
				,null,null,null));
		uiManager.configBusinessIdentifiable(StudentClassroomSession.class, null);
		webNavigationManager.useDynamicSelectView(StudentClassroomSession.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionEditPage.One.class, StudentClassroomSessionDivisionDetails.class
				,null,StudentClassroomSessionDivisionEditPage.Many.class,StudentClassroomSessionDivisionQueryManyFormModel.class));
		uiManager.configBusinessIdentifiable(StudentClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(StudentClassroomSessionDivision.class);
		AbstractSelectManyPage.Listener.COLLECTION.add(new StudentClassroomSessionDivisionQueryManyFormModel.PageAdapter());
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(StudentClassroomSessionDivisionSubject.class, StudentSubjectEditPage.Form.class, StudentClassroomSessionDivisionSubjectDetails.class
				,null,null,null));
		uiManager.configBusinessIdentifiable(StudentClassroomSessionDivisionSubject.class, null);
		webNavigationManager.useDynamicSelectView(StudentClassroomSessionDivisionSubject.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(Evaluation.class, null, EvaluationDetails.class
				,null,null,null));
		uiManager.configBusinessIdentifiable(Evaluation.class, null);
		
		/*if(Boolean.TRUE.equals(SchoolWebManager.EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
			SecurityFilter.addUniformResourceLocatorRuntimeConstraint(new UniformResourceLocator("/private/__role__/__manager__/evaluation/edit.jsf")
				,new UniformResourceLocatorRuntimeConstraint(){
					@Override
					public Boolean isAccessibleByUserAccount(AbstractUserSession<?,?> userSession,UserAccount userAccount, UniformResourceLocator uniformResourceLocator, HttpServletRequest request,HttpServletResponse response) {
						Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userAccount.getUser());
						if(teacher==null)
							return Boolean.FALSE;
						ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = webManager
								.getIdentifiableFromRequestParameter(request, ClassroomSessionDivisionSubjectEvaluationType.class,Boolean.TRUE);
						return classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getTeacher()!=null 
								&& classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getTeacher().equals(teacher);
					}
				});
		}*/
		
		/*
		if(Boolean.TRUE.equals(SchoolWebManager.APPRECIATION_EDITABLE_BY_COODINATOR_ONLY)){
			SecurityFilter.addUniformResourceLocatorRuntimeConstraint(new UniformResourceLocator("/private/studentclassroomsessiondivision/edit.jsf")
				,new UniformResourceLocatorRuntimeConstraint(){
					@Override
					public Boolean isAccessibleByUserAccount(AbstractUserSession userSession,UserAccount userAccount, UniformResourceLocator uniformResourceLocator, HttpServletRequest request,HttpServletResponse response) {
						Teacher teacher = inject(TeacherBusiness.class).findByPerson((Person) userAccount.getUser());
						if(teacher==null)
							return Boolean.FALSE;
						ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = webManager
								.getIdentifiableFromRequestParameter(request, ClassroomSessionDivisionSubjectEvaluationType.class,Boolean.TRUE);
						return classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getTeacher()!=null 
								&& classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getTeacher().equals(teacher);
					}
				});
		}*/
	}
		
}
