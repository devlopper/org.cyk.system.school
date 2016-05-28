package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.cyk.system.company.ui.web.primefaces.AbstractCompanyContextListener;
import org.cyk.system.root.model.network.UniformResourceLocator;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.AcademicSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectEvaluationTypeDetails;
import org.cyk.system.school.business.impl.subject.EvaluationDetails;
import org.cyk.system.school.business.impl.subject.StudentSubjectDetails;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentSubject;
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
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionQueryManyFormModel;
import org.cyk.system.school.ui.web.primefaces.session.StudentSubjectEditPage;
import org.cyk.ui.api.AbstractUserSession;
import org.cyk.ui.api.config.IdentifiableConfiguration;
import org.cyk.ui.web.api.AbstractWebPage;
import org.cyk.ui.web.api.servlet.SecurityFilter;
import org.cyk.ui.web.api.servlet.SecurityFilter.UniformResourceLocatorRuntimeConstraint;

public abstract class AbstractSchoolContextListener extends AbstractCompanyContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	/*@Inject*/ protected SchoolBusinessLayer schoolBusinessLayer;
	@Inject protected SchoolWebManager schoolWebManager;
		
	@Override
	protected void initialisation() {
		super.initialisation();
		AbstractWebPage.DEFAULT_LAYOUT.setWest("/org.cyk.ui.web.primefaces.school/include/layout/westtop.xhtml");
	}
	
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		//uiManager.businessEntityInfos(ClassroomSession.class).setUiConsultViewId("");
		//IdentifiableConfiguration identifiableConfiguration = uiManager.findConfiguration(ClassroomSession.class);
		//identifiableConfiguration.setForms(ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(AcademicSession.class, AcademicSessionEditPage.Form.class, AcademicSessionDetails.class,null,null,null));
		uiManager.configBusinessIdentifiable(AcademicSession.class, null);
		//webNavigationManager.useDynamicSelectView(AcademicSession.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSession.class, ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class,ClassroomSessionQueryOneFormModel.class,null,ClassroomSessionQueryManyFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSession.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSession.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivision.class, ClassroomSessionDivisionEditPage.Form.class, ClassroomSessionDivisionDetails.class
				,ClassroomSessionDivisionQueryOneFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivision.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectEditPage.Form.class, ClassroomSessionDivisionSubjectDetails.class
				,ClassroomSessionDivisionSubjectQueryFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubject.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubject.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubjectEvaluationType.class, ClassroomSessionDivisionSubjectEvaluationTypeEditPage.Form.class, ClassroomSessionDivisionSubjectEvaluationTypeDetails.class
				,ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.class,null,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubjectEvaluationType.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubjectEvaluationType.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionEditPage.One.class, StudentClassroomSessionDivisionDetails.class
				,null,StudentClassroomSessionDivisionEditPage.Many.class,StudentClassroomSessionDivisionQueryManyFormModel.class));
		uiManager.configBusinessIdentifiable(StudentClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(StudentClassroomSessionDivision.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(StudentSubject.class, StudentSubjectEditPage.Form.class, StudentSubjectDetails.class
				,null,null,null));
		uiManager.configBusinessIdentifiable(StudentSubject.class, null);
		webNavigationManager.useDynamicSelectView(StudentSubject.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(Evaluation.class, null, EvaluationDetails.class
				,null,null,null));
		uiManager.configBusinessIdentifiable(Evaluation.class, null);
		
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionQueryOneFormModel.PageAdapter());
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionDivisionQueryOneFormModel.PageAdapter());
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionDivisionSubjectQueryFormModel.PageAdapter());
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.PageAdapter());
		
		primefacesManager.getSelectManyPageListeners().add(new ClassroomSessionQueryManyFormModel.PageAdapter());
		primefacesManager.getSelectManyPageListeners().add(new StudentClassroomSessionDivisionQueryManyFormModel.PageAdapter());

		primefacesManager.getProcessManyPageListeners().add(new ClassroomSessionQueryManyFormModel.ProcessPageAdapter());
		
		if(Boolean.TRUE.equals(SchoolWebManager.EVALUATION_EDITABLE_BY_TEACHER_ONLY)){
			SecurityFilter.addUniformResourceLocatorRuntimeConstraint(new UniformResourceLocator("/private/__role__/__manager__/evaluation/edit.jsf")
				,new UniformResourceLocatorRuntimeConstraint(){
					@Override
					public Boolean isAccessibleByUserAccount(AbstractUserSession<?,?> userSession,UserAccount userAccount, UniformResourceLocator uniformResourceLocator, HttpServletRequest request,HttpServletResponse response) {
						Teacher teacher = schoolBusinessLayer.getTeacherBusiness().findByPerson((Person) userAccount.getUser());
						if(teacher==null)
							return Boolean.FALSE;
						ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = webManager
								.getIdentifiableFromRequestParameter(request, ClassroomSessionDivisionSubjectEvaluationType.class,Boolean.TRUE);
						return classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getTeacher()!=null 
								&& classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject().getTeacher().equals(teacher);
					}
				});
		}
		
		/*
		if(Boolean.TRUE.equals(SchoolWebManager.APPRECIATION_EDITABLE_BY_COODINATOR_ONLY)){
			SecurityFilter.addUniformResourceLocatorRuntimeConstraint(new UniformResourceLocator("/private/studentclassroomsessiondivision/edit.jsf")
				,new UniformResourceLocatorRuntimeConstraint(){
					@Override
					public Boolean isAccessibleByUserAccount(AbstractUserSession userSession,UserAccount userAccount, UniformResourceLocator uniformResourceLocator, HttpServletRequest request,HttpServletResponse response) {
						Teacher teacher = SchoolBusinessLayer.getInstance().getTeacherBusiness().findByPerson((Person) userAccount.getUser());
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
	
	@Override
	protected void businessAdapters(ServletContextEvent event) {
		super.businessAdapters(event);
		
	}
	
}
