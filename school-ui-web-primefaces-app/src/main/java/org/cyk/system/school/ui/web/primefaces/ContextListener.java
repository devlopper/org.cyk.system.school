package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;
import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

import org.cyk.system.root.business.api.BusinessAdapter;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.ui.web.primefaces.api.RootWebManager;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.ui.web.primefaces.registration.MedicalInformationsFormModel;
import org.cyk.ui.web.primefaces.AbstractContextListener;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormPageAdapter;
import org.cyk.ui.web.primefaces.page.tools.MedicalInformationsEditPage;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.computation.DataReadConfiguration;

@WebListener
public class ContextListener extends AbstractContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;

	@Inject private StudentBusiness studentBusiness;
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		primefacesManager.getBusinessEntityFormPageListeners().add(new BusinessEntityFormPageAdapter<Student>(Student.class){
			private static final long serialVersionUID = -823942826619424098L;
			@Override
			public void initialisationEnded(AbstractBean bean) {
				super.initialisationEnded(bean);
				if(bean instanceof MedicalInformationsEditPage){
					((MedicalInformationsEditPage)bean).setFormModelClass(MedicalInformationsFormModel.class);
				}
			}
		});
	}
	
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		uiManager.registerApplicationUImanager(RootWebManager.getInstance());
		uiManager.registerApplicationUImanager(SchoolWebManager.getInstance());
		
		uiManager.getBusinesslisteners().add(new BusinessAdapter(){
			private static final long serialVersionUID = 4605368263736933413L;
			@SuppressWarnings("unchecked")
			@Override
			public <T extends AbstractIdentifiable> Collection<T> find(Class<T> dataClass, DataReadConfiguration configuration) {
				if(Student.class.equals(dataClass)){
					return (Collection<T>) studentBusiness.findAll();
				}
				return super.find(dataClass, configuration);
			}
			
			@Override
			public <T extends AbstractIdentifiable> Long count(Class<T> dataClass, DataReadConfiguration configuration) {
				if(Student.class.equals(dataClass)){
					return studentBusiness.countAll();
				}
				return super.count(dataClass, configuration);
			}
		});	
	}
	
}
