package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;

import org.cyk.system.root.business.api.BusinessAdapter;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.ui.web.primefaces.api.RootWebManager;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.ui.web.api.AbstractWebPage;
import org.cyk.ui.web.primefaces.AbstractContextListener;
import org.cyk.utility.common.computation.DataReadConfiguration;

public abstract class AbstractSchoolContextListener extends AbstractContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	@Inject protected SchoolBusinessLayer schoolBusinessLayer;
	
	@Inject protected StudentBusiness studentBusiness;
	@Inject protected TeacherBusiness teacherBusiness;
	@Inject protected ClassroomSessionBusiness classroomSessionBusiness;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		AbstractWebPage.DEFAULT_LAYOUT.setWest("/org.cyk.ui.web.primefaces.school/include/layout/westtop.xhtml");
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
					return (Collection<T>) studentBusiness.findAll();
				}else if(Teacher.class.equals(dataClass)){
					return (Collection<T>) teacherBusiness.findAll();
				}else if(ClassroomSession.class.equals(dataClass)){
					return (Collection<T>) classroomSessionBusiness.findAll();
				}
				return super.find(dataClass, configuration);
			}
			
			@Override
			public <T extends AbstractIdentifiable> Long count(Class<T> dataClass, DataReadConfiguration configuration) {
				if(Student.class.equals(dataClass)){
					return studentBusiness.countAll();
				}else if(Teacher.class.equals(dataClass)){
					return teacherBusiness.countAll();
				}else if(ClassroomSession.class.equals(dataClass)){
					return classroomSessionBusiness.countAll();
				}
				return super.count(dataClass, configuration);
			}
		});	
	}
	
	@Override
	protected void applicationUImanagers(ServletContextEvent event) {
		super.applicationUImanagers(event);
		uiManager.registerApplicationUImanager(RootWebManager.getInstance());
		uiManager.registerApplicationUImanager(SchoolWebManager.getInstance());
	}
	
}
