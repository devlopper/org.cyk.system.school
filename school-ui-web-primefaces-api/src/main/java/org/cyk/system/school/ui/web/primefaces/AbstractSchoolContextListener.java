package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.net.URL;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;

import org.cyk.system.company.ui.web.primefaces.AbstractCompanyContextListener;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectEvaluationTypeDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEvaluationTypeEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionQueryManyFormModel;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionQueryManyFormModel;
import org.cyk.ui.api.config.IdentifiableConfiguration;
import org.cyk.ui.web.api.AbstractWebPage;
import org.cyk.ui.web.api.servlet.SecurityFilter;

public abstract class AbstractSchoolContextListener extends AbstractCompanyContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	@Inject protected SchoolBusinessLayer schoolBusinessLayer;
		
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
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSession.class, ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class,null,ClassroomSessionQueryManyFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSession.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSession.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivision.class, ClassroomSessionDivisionEditPage.Form.class, ClassroomSessionDivisionDetails.class
				,ClassroomSessionDivisionQueryFormModel.class,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivision.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectEditPage.Form.class, ClassroomSessionDivisionSubjectDetails.class
				,ClassroomSessionDivisionSubjectQueryFormModel.class,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubject.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubject.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubjectEvaluationType.class, ClassroomSessionDivisionSubjectEvaluationTypeEditPage.Form.class, ClassroomSessionDivisionSubjectEvaluationTypeDetails.class
				,ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.class,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubjectEvaluationType.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubjectEvaluationType.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionEditPage.Form.class, StudentClassroomSessionDivisionDetails.class
				,null,StudentClassroomSessionDivisionQueryManyFormModel.class));
		uiManager.configBusinessIdentifiable(StudentClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(StudentClassroomSessionDivision.class);
		
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionDivisionQueryFormModel.PageAdapter());
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionDivisionSubjectQueryFormModel.PageAdapter());
		primefacesManager.getSelectOnePageListeners().add(new ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.PageAdapter());
		
		primefacesManager.getSelectManyPageListeners().add(new ClassroomSessionQueryManyFormModel.PageAdapter());
		primefacesManager.getSelectManyPageListeners().add(new StudentClassroomSessionDivisionQueryManyFormModel.PageAdapter());

		//SecurityFilter.URL_CONSTRAINTS.put("/private/__role__/__manager__/evaluation/edit.jsf",new SubjectEvaluationEditPage.SecurityConstraint());
		
		SecurityFilter.Listener.COLLECTION.add(new SecurityFilter.Listener.Adapter.Default(){
			private static final long serialVersionUID = 4605368263736933413L;
			
			@Override
			public Boolean isUrlAccessible(URL url) {
				return super.isUrlAccessible(url);
			}
		});
	}
	
	@Override
	protected void businessAdapters(ServletContextEvent event) {
		super.businessAdapters(event);
		
	}
	
}
