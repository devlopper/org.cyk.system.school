package org.cyk.system.school.ui.web.primefaces.iesa;

import java.io.Serializable;
import java.lang.reflect.Field;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.ui.web.primefaces.iesa.ContextListener.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionDivisionConsultPage;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.api.model.party.AbstractPersonEditFormModel;
import org.cyk.ui.web.primefaces.Table.ColumnAdapter;
import org.cyk.ui.web.primefaces.UserSession;
import org.cyk.ui.web.primefaces.page.AbstractPrimefacesPage;
import org.cyk.ui.web.primefaces.page.DetailsConfiguration;

public class PrimefacesManager extends org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager implements Serializable {

	private static final long serialVersionUID = -8716834916609095637L;
	
	public PrimefacesManager() {
		getFormConfiguration(Student.class, Crud.CREATE).deleteRequiredFieldNames(StudentEditPage.Form.FIELD_CODE);
		
		getFormConfiguration(Student.class, Crud.READ).addFieldNames(AbstractPersonEditFormModel.FIELD_CODE,AbstractPersonEditFormModel.FIELD_NAME
				,AbstractPersonEditFormModel.FIELD_LAST_NAMES,AbstractPersonEditFormModel.FIELD_BIRTH_DATE,AbstractPersonEditFormModel.FIELD_SEX
				,AbstractPersonEditFormModel.FIELD_IMAGE);
		
		getFormConfiguration(Person.class, Crud.READ).addFieldNames(AbstractPersonEditFormModel.FIELD_CODE,AbstractPersonEditFormModel.FIELD_NAME
				,AbstractPersonEditFormModel.FIELD_LAST_NAMES,AbstractPersonEditFormModel.FIELD_BIRTH_DATE,AbstractPersonEditFormModel.FIELD_SEX
				,AbstractPersonEditFormModel.FIELD_IMAGE);
	}
	
	@Override
	public SystemMenu getSystemMenu(UserSession userSession) {
		return SystemMenuBuilder.getInstance().build(userSession);
	}
	
	@Override
	protected void configureStudentClassroomSessionDivisionSubjectClass() {
		super.configureStudentClassroomSessionDivisionSubjectClass();
		registerDetailsConfiguration(AbstractStudentClassroomSessionDivisionSubjectDetails.class, new StudentClassroomSessionDivisionSubjectDetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,final AbstractPrimefacesPage page) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						if(page instanceof StudentClassroomSessionDivisionConsultPage)
							return isFieldNameIn(field,StudentClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
								,StudentClassroomSessionDivisionSubjectDetails.FIELD_TEST1,StudentClassroomSessionDivisionSubjectDetails.FIELD_TEST2
								,StudentClassroomSessionDivisionSubjectDetails.FIELD_EXAM);
						return isFieldNameIn(field, StudentClassroomSessionDivisionSubjectDetails.FIELD_STUDENT);
					}
				};
			}
		});
	}
	
}
