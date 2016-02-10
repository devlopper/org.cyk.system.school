package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;

import org.cyk.system.company.ui.web.primefaces.AbstractCompanyContextListener;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectEvaluationTypeDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEvaluationTypeEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionEditPage;
import org.cyk.ui.api.config.IdentifiableConfiguration;
import org.cyk.ui.web.api.AbstractWebPage;

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
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSession.class, ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class,null));
		uiManager.configBusinessIdentifiable(ClassroomSession.class, null);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivision.class, ClassroomSessionDivisionEditPage.Form.class, ClassroomSessionDivisionDetails.class
				,ClassroomSessionDivisionQueryFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivision.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivision.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectEditPage.Form.class, ClassroomSessionDivisionSubjectDetails.class
				,ClassroomSessionDivisionSubjectQueryFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubject.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubject.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubjectEvaluationType.class, ClassroomSessionDivisionSubjectEvaluationTypeEditPage.Form.class, ClassroomSessionDivisionSubjectEvaluationTypeDetails.class
				,ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubjectEvaluationType.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubjectEvaluationType.class);
		
		primefacesManager.getSelectPageListeners().add(new ClassroomSessionDivisionQueryFormModel.PageAdapter());
		primefacesManager.getSelectPageListeners().add(new ClassroomSessionDivisionSubjectQueryFormModel.PageAdapter());
		primefacesManager.getSelectPageListeners().add(new ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel.PageAdapter());
		
	}
	
	@Override
	protected void businessAdapters(ServletContextEvent event) {
		super.businessAdapters(event);
		
	}
	
}
