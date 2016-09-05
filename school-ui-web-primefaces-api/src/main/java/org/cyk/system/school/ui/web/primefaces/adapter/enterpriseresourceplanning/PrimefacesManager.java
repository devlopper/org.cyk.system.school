package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;
import java.lang.reflect.Field;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.business.impl.session.AcademicSessionDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDetails;
import org.cyk.system.school.business.impl.session.LevelTimeDivisionDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.system.school.ui.web.primefaces.session.AcademicSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionEditPage;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.web.primefaces.UserSession;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.DetailsConfiguration;

public class PrimefacesManager extends org.cyk.system.company.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager implements Serializable {

	private static final long serialVersionUID = -8716834916609095637L;
	
	public PrimefacesManager() {
		configureAcademicSessionClass();
		configureLevelTimeDivisionClass();
		configureClassroomSessionClass();
		
		configureStudentClass();
		configureStudentClassroomSessionClass();
		
	}
	
	@Override
	public SystemMenu getSystemMenu(UserSession userSession) {
		return SystemMenuBuilder.getInstance().build(userSession);
	}
	
	protected void configureAcademicSessionClass() {
		getFormConfiguration(AcademicSession.class, Crud.CREATE).addFieldNames(AcademicSessionEditPage.Form.FIELD_AGGREGATE_ATTENDANCE
				,AcademicSessionEditPage.Form.FIELD_ATTENDANCE_TIME_DIVISION_TYPE,AcademicSessionEditPage.Form.FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE
				,AcademicSessionEditPage.Form.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX,AcademicSessionEditPage.Form.FIELD_FROM_DATE
				,AcademicSessionEditPage.Form.FIELD_NEXT_STARTING_DATE,AcademicSessionEditPage.Form.FIELD_TO_DATE);
		registerDetailsConfiguration(AcademicSessionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof AcademicSessionDetails)
							return isFieldNameIn(field,AcademicSessionDetails.FIELD_FROM_DATE,AcademicSessionDetails.FIELD_TO_DATE
									,AcademicSessionDetails.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX);
						
						return super.build(data, field);
					}
				};
			}
		});
	}
	
	protected void configureClassroomSessionClass() {
		getFormConfiguration(ClassroomSession.class, Crud.CREATE).addRequiredFieldNames(ClassroomSessionEditPage.Form.FIELD_ACADEMIC_SESSION
				,ClassroomSessionEditPage.Form.FIELD_LEVEL_TIME_DIVISION).addFieldNames(ClassroomSessionEditPage.Form.FIELD_COORDINATOR
				,ClassroomSessionEditPage.Form.FIELD_SUFFIX);
		registerDetailsConfiguration(ClassroomSessionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof ClassroomSessionDetails)
							return isFieldNameIn(field,ClassroomSessionDetails.FIELD_LEVEL_TIME_DIVISION,ClassroomSessionDetails.FIELD_SUFFIX
									,ClassroomSessionDetails.FIELD_COORDINATOR,ClassroomSessionDetails.FIELD_NUMBER_OF_STUDENTS);
						
						return super.build(data, field);
					}
				};
			}
		});
	}
	
	protected void configureLevelTimeDivisionClass() {
		
		registerDetailsConfiguration(LevelTimeDivisionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof LevelTimeDivisionDetails)
							return isFieldNameIn(field,LevelTimeDivisionDetails.FIELD_LEVEL,LevelTimeDivisionDetails.FIELD_TIME_DIVISION_TYPE
									,LevelTimeDivisionDetails.FIELD_INDEX);
						
						return super.build(data, field);
					}
				};
			}
		});
	}
	
	protected void configureStudentClass() {
		getFormConfiguration(Student.class, Crud.CREATE).addRequiredFieldNames(StudentEditPage.Form.FIELD_CODE)
		.addFieldNames(StudentEditPage.Form.FIELD_CLASSROOMSESSION,StudentEditPage.Form.FIELD_IMAGE,StudentEditPage.Form.FIELD_NAME,StudentEditPage.Form.FIELD_LAST_NAMES
				,StudentEditPage.Form.FIELD_BIRTH_DATE,StudentEditPage.Form.FIELD_BIRTH_LOCATION,StudentEditPage.Form.FIELD_NATIONALITY,StudentEditPage.Form.FIELD_SEX
				,StudentEditPage.Form.FIELD_BLOOD_GROUP,StudentEditPage.Form.FIELD_LANGUAGE_COLLECTION,StudentEditPage.Form.FIELD_REGISTRATION_DATE);
		
		getFormConfiguration(Student.class, Crud.UPDATE).addRequiredFieldNames(StudentEditPage.Form.FIELD_CODE)
		.addFieldNames(StudentEditPage.Form.FIELD_IMAGE,StudentEditPage.Form.FIELD_NAME,StudentEditPage.Form.FIELD_LAST_NAMES
				,StudentEditPage.Form.FIELD_BIRTH_DATE,StudentEditPage.Form.FIELD_BIRTH_LOCATION,StudentEditPage.Form.FIELD_NATIONALITY,StudentEditPage.Form.FIELD_SEX
				,StudentEditPage.Form.FIELD_BLOOD_GROUP,StudentEditPage.Form.FIELD_LANGUAGE_COLLECTION,StudentEditPage.Form.FIELD_REGISTRATION_DATE);
		
		getFormConfiguration(Student.class, Crud.DELETE).addFieldNames(StudentEditPage.Form.FIELD_CODE,StudentEditPage.Form.FIELD_IMAGE,StudentEditPage.Form.FIELD_NAME
				,StudentEditPage.Form.FIELD_LAST_NAMES);
		
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
	
	protected void configureStudentClassroomSessionClass() {
		getFormConfiguration(StudentClassroomSession.class, Crud.CREATE).addRequiredFieldNames(StudentClassroomSessionEditPage.Form.FIELD_CLASSROOM_STUDENT
				,StudentClassroomSessionEditPage.Form.FIELD_CLASSROOM_SESSION);
		registerDetailsConfiguration(StudentClassroomSessionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						//if(data instanceof StudentClassroomSessionDetails)
							return isFieldNameIn(field,StudentClassroomSessionDetails.FIELD_STUDENT,StudentClassroomSessionDetails.FIELD_CLASSROOM_SESSION);
						
					}
				};
			}
		});
	}
}
