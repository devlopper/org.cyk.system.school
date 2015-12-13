package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;

import org.cyk.system.company.ui.web.primefaces.AbstractCompanyContextListener;
import org.cyk.system.root.business.api.BusinessAdapter;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectQueryFormModel;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionEditPage;
import org.cyk.ui.api.config.IdentifiableConfiguration;
import org.cyk.ui.web.api.AbstractWebPage;
import org.cyk.utility.common.computation.DataReadConfiguration;

public abstract class AbstractSchoolContextListener extends AbstractCompanyContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	@Inject protected SchoolBusinessLayer schoolBusinessLayer;
		
	@Override
	protected void initialisation() {
		super.initialisation();
		AbstractWebPage.DEFAULT_LAYOUT.setWest("/org.cyk.ui.web.primefaces.school/include/layout/westtop.xhtml");
	}
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		primefacesManager.getSelectPageListeners().add(new ClassroomSessionDivisionSubjectQueryFormModel.PageAdapter());
	}
	
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		//uiManager.businessEntityInfos(ClassroomSession.class).setUiConsultViewId("");
		//IdentifiableConfiguration identifiableConfiguration = uiManager.findConfiguration(ClassroomSession.class);
		//identifiableConfiguration.setForms(ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSession.class, ClassroomSessionEditPage.Form.class, ClassroomSessionDetails.class,null));
		uiManager.configBusinessIdentifiable(ClassroomSession.class, null);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivision.class, ClassroomSessionDivisionEditPage.Form.class, ClassroomSessionDivisionDetails.class,null));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivision.class, null);
		
		uiManager.registerConfiguration(new IdentifiableConfiguration(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectEditPage.Form.class, ClassroomSessionDivisionSubjectDetails.class
				,ClassroomSessionDivisionSubjectQueryFormModel.class));
		uiManager.configBusinessIdentifiable(ClassroomSessionDivisionSubject.class, null);
		webNavigationManager.useDynamicSelectView(ClassroomSessionDivisionSubject.class);
	}
	
	@Override
	protected void businessAdapters(ServletContextEvent event) {
		super.businessAdapters(event);
		uiManager.getBusinesslisteners().add(new BusinessAdapter(){
			private static final long serialVersionUID = 4605368263736933413L;
			@SuppressWarnings("unchecked")
			@Override
			public <T extends AbstractIdentifiable> Collection<T> find(Class<T> dataClass, DataReadConfiguration configuration) {
				if(Student.class.equals(dataClass)){
					return (Collection<T>) schoolBusinessLayer.getStudentBusiness().findAll();
				}else if(Teacher.class.equals(dataClass)){
					return (Collection<T>) schoolBusinessLayer.getTeacherBusiness().findAll();
				}else if(ClassroomSession.class.equals(dataClass)){
					return (Collection<T>) schoolBusinessLayer.getClassroomSessionBusiness().findAll();
				}
				return super.find(dataClass, configuration);
			}
			
			@Override
			public <T extends AbstractIdentifiable> Long count(Class<T> dataClass, DataReadConfiguration configuration) {
				if(Student.class.equals(dataClass)){
					return schoolBusinessLayer.getStudentBusiness().countAll();
				}else if(Teacher.class.equals(dataClass)){
					return schoolBusinessLayer.getTeacherBusiness().countAll();
				}else if(ClassroomSession.class.equals(dataClass)){
					return schoolBusinessLayer.getClassroomSessionBusiness().countAll();
				}
				return super.count(dataClass, configuration);
			}
		});	
	}
	
}
