package org.cyk.system.school.ui.web.primefaces.iesa;

import java.io.Serializable;
import java.lang.reflect.Field;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.UserSession;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.DetailsConfiguration;

public class PrimefacesManager extends org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager implements Serializable {

	private static final long serialVersionUID = -8716834916609095637L;
	
	public PrimefacesManager() {
		configureStudentModule();
	}
	
	@Override
	public SystemMenu getSystemMenu(UserSession userSession) {
		return SystemMenuBuilder.getInstance().build(userSession);
	}
	
	protected void configureStudentModule() {
		getFormConfiguration(Student.class, Crud.CREATE).addRequiredFieldNames(StudentEditPage.Form.FIELD_CODE)
		.addFieldNames(StudentEditPage.Form.FIELD_CLASSROOMSESSION,StudentEditPage.Form.FIELD_IMAGE,StudentEditPage.Form.FIELD_NAME,StudentEditPage.Form.FIELD_LAST_NAMES
				,StudentEditPage.Form.FIELD_BIRTH_DATE,StudentEditPage.Form.FIELD_BIRTH_LOCATION,StudentEditPage.Form.FIELD_NATIONALITY,StudentEditPage.Form.FIELD_SEX
				,StudentEditPage.Form.FIELD_BLOOD_GROUP,StudentEditPage.Form.FIELD_LANGUAGE_COLLECTION,StudentEditPage.Form.FIELD_REGISTRATION_DATE);
		registerDetailsConfiguration(StudentDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						return isFieldNameIn(field,StudentDetails.FIELD_CODE,StudentDetails.FIELD_NAME,StudentDetails.FIELD_LASTNAMES
								,StudentDetails.FIELD_BIRTH_DATE,StudentDetails.FIELD_BIRTH_LOCATION,StudentDetails.FIELD_REGISTRATION_DATE
								,StudentDetails.FIELD_SEX,StudentDetails.FIELD_BLOOD_GROUP,StudentDetails.FIELD_LANGUAGE_COLLECTION,StudentDetails.FIELD_CLASSROOMSESSION);
					}
				};
			}
		});
		
	}
}
