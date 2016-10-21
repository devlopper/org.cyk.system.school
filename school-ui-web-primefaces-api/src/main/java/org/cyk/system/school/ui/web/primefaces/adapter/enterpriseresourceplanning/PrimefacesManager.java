package org.cyk.system.school.ui.web.primefaces.adapter.enterpriseresourceplanning;

import java.io.Serializable;
import java.lang.reflect.Field;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.impl.time.PeriodDetails;
import org.cyk.system.school.business.impl.actor.StudentDetails;
import org.cyk.system.school.business.impl.actor.TeacherDetails;
import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails;
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
import org.cyk.system.school.business.impl.subject.StudentClassroomSessionDivisionSubjectEvaluationDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.ui.web.primefaces.StudentResultsMetricValueDetails;
import org.cyk.system.school.ui.web.primefaces.page.StudentEditPage;
import org.cyk.system.school.ui.web.primefaces.page.TeacherEditPage;
import org.cyk.system.school.ui.web.primefaces.session.AcademicSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionSubjectEditPage;
import org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.EvaluationEditPage;
import org.cyk.system.school.ui.web.primefaces.session.LevelTimeDivisionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.SubjectClassroomSessionEditPage;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionEditPage;
import org.cyk.ui.api.command.menu.SystemMenu;
import org.cyk.ui.api.model.geography.LocationFormModel;
import org.cyk.ui.api.model.language.LanguageCollectionFormModel;
import org.cyk.ui.web.primefaces.Table.ColumnAdapter;
import org.cyk.ui.web.primefaces.UserSession;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.AbstractPrimefacesPage;
import org.cyk.ui.web.primefaces.page.DetailsConfiguration;

public class PrimefacesManager extends org.cyk.system.company.ui.web.primefaces.adapter.enterpriseresourceplanning.PrimefacesManager implements Serializable {

	private static final long serialVersionUID = -8716834916609095637L;
	
	public PrimefacesManager() {
		configureAcademicSessionClass();
		configureLevelTimeDivisionClass();
		
		configureClassroomSessionClass();
		configureSubjectClassroomSessionClass();
		
		configureClassroomSessionDivisionClass();
		configureClassroomSessionDivisionSubjectClass();
		configureClassroomSessionDivisionSubjectEvaluationTypeClass();
		
		configureStudentClass();
		configureStudentClassroomSessionClass();
		configureStudentClassroomSessionDivisionClass();
		configureStudentClassroomSessionDivisionSubjectClass();
		configureStudentClassroomSessionDivisionSubjectEvaluationClass();
		configureStudentResultsMetricValueDetailsClass();
		
		configureTeacherClass();
		
		configureEvaluationClass();
		
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
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof AcademicSessionDetails)
							return isFieldNameIn(field,AcademicSessionDetails.FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX,AcademicSessionDetails.FIELD_EXISTENCE_PERIOD
									,AcademicSessionDetails.FIELD_AGGREGATE_ATTENDANCE,AcademicSessionDetails.FIELD_ATTENDANCE_TIME_DIVISION_TYPE
									,AcademicSessionDetails.FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE,AcademicSessionDetails.FIELD_NEXT_STARTING_DATE);
						if(data instanceof PeriodDetails)
							return isFieldNameIn(field,PeriodDetails.FIELD_FROM_DATE,PeriodDetails.FIELD_TO_DATE);
						return Boolean.FALSE;
					}
				};
			}
		});
	}
	
	protected void configureLevelTimeDivisionClass() {
		getFormConfiguration(LevelTimeDivision.class, Crud.UPDATE).addFieldNames(LevelTimeDivisionEditPage.Form.FIELD_CODE,LevelTimeDivisionEditPage.Form.FIELD_LEVEL
				,LevelTimeDivisionEditPage.Form.FIELD_TIME_DIVISION_TYPE,LevelTimeDivisionEditPage.Form.FIELD_ORDER_NUMBER);
		
		getFormConfiguration(LevelTimeDivision.class, Crud.READ).addFieldNames(LevelTimeDivisionDetails.FIELD_CODE,LevelTimeDivisionDetails.FIELD_LEVEL
				,LevelTimeDivisionDetails.FIELD_TIME_DIVISION_TYPE,LevelTimeDivisionDetails.FIELD_ORDER_NUMBER);
		
		registerDetailsConfiguration(LevelTimeDivisionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof LevelTimeDivisionDetails)
							return isFieldNameIn(field,LevelTimeDivisionDetails.FIELD_CODE,LevelTimeDivisionDetails.FIELD_LEVEL
									,LevelTimeDivisionDetails.FIELD_TIME_DIVISION_TYPE,LevelTimeDivisionDetails.FIELD_ORDER_NUMBER);
						return Boolean.FALSE;
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
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof ClassroomSessionDetails)
							return isFieldNameIn(field,ClassroomSessionDetails.FIELD_LEVEL_TIME_DIVISION,ClassroomSessionDetails.FIELD_SUFFIX
									,ClassroomSessionDetails.FIELD_COORDINATOR,ClassroomSessionDetails.FIELD_NUMBER_OF_STUDENT);
						return Boolean.FALSE;
					}
				};
			}
		});
	}
	
	protected void configureSubjectClassroomSessionClass() {
		getFormConfiguration(SubjectClassroomSession.class, Crud.CREATE).addRequiredFieldNames(SubjectClassroomSessionEditPage.Form.FIELD_CLASSROOM_SESSION,
				SubjectClassroomSessionEditPage.Form.FIELD_SUBJECT)
					.addFieldNames(SubjectClassroomSessionEditPage.Form.FIELD_TEACHER);
		
		registerDetailsConfiguration(SubjectClassroomSessionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof SubjectClassroomSessionDetails)
							return isFieldNameIn(field,SubjectClassroomSessionDetails.FIELD_CLASSROOM_SESSION,SubjectClassroomSessionDetails.FIELD_SUBJECT
									,SubjectClassroomSessionDetails.FIELD_TEACHER);
						return Boolean.FALSE;
					}
				};
			}
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,AbstractPrimefacesPage page) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field, SubjectClassroomSessionDetails.FIELD_SUBJECT, SubjectClassroomSessionDetails.FIELD_TEACHER);
					}
				};
			}
		});
	}
	
	protected void configureClassroomSessionDivisionClass() {
		getFormConfiguration(ClassroomSessionDivision.class, Crud.READ).addFieldNames(ClassroomSessionDivisionDetails.FIELD_CLASSROOM_SESSION,ClassroomSessionDivisionDetails.FIELD_ORDER_NUMBER
				,ClassroomSessionDivisionDetails.FIELD_WEIGHT);
		
		registerDetailsConfiguration(ClassroomSessionDivisionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof ClassroomSessionDivisionDetails)
							return isFieldNameIn(field,ClassroomSessionDivisionDetails.FIELD_CLASSROOM_SESSION,ClassroomSessionDivisionDetails.FIELD_ORDER_NUMBER
									,ClassroomSessionDivisionDetails.FIELD_WEIGHT);
						return Boolean.FALSE;
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,AbstractPrimefacesPage page) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,ClassroomSessionDivisionDetails.FIELD_NAME);
					}
				};
			}
		});
	}
	
	protected void configureClassroomSessionDivisionSubjectClass() {
		getFormConfiguration(ClassroomSessionDivisionSubject.class, Crud.CREATE).addFieldNames(ClassroomSessionDivisionSubjectEditPage.Form.FIELD_CLASSROOM_SESSION
				,ClassroomSessionDivisionSubjectEditPage.Form.FIELD_CLASSROOM_SESSION_DIVISION
				,ClassroomSessionDivisionSubjectEditPage.Form.FIELD_SUBJECT,ClassroomSessionDivisionSubjectEditPage.Form.FIELD_TEACHER);
		
		registerDetailsConfiguration(ClassroomSessionDivisionSubjectDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof ClassroomSessionDivisionSubjectDetails)
							return isFieldNameIn(field,ClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION,ClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION_DIVISION
									,ClassroomSessionDivisionSubjectDetails.FIELD_SUBJECT,ClassroomSessionDivisionSubjectDetails.FIELD_TEACHER);
						return Boolean.FALSE;
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,AbstractPrimefacesPage page) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field, ClassroomSessionDivisionSubjectDetails.FIELD_SUBJECT,ClassroomSessionDivisionSubjectDetails.FIELD_TEACHER);
					}
				};
			}
		});
	}
	 
	protected void configureClassroomSessionDivisionSubjectEvaluationTypeClass() {
		registerDetailsConfiguration(ClassroomSessionDivisionSubjectEvaluationTypeDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						if(data instanceof ClassroomSessionDivisionSubjectEvaluationTypeDetails)
							return isFieldNameIn(field,ClassroomSessionDivisionSubjectEvaluationTypeDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
									,ClassroomSessionDivisionSubjectEvaluationTypeDetails.FIELD_WEIGHT);
						return Boolean.FALSE;
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,AbstractPrimefacesPage page) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field, ClassroomSessionDivisionSubjectEvaluationTypeDetails.FIELD_WEIGHT);
					}
				};
			}
		});
	}
	
	protected void configureStudentClass() {
		getFormConfiguration(Student.class, Crud.CREATE).addRequiredFieldNames(StudentEditPage.Form.FIELD_CODE)
		.addFieldNames(StudentEditPage.Form.FIELD_ADMISSION_LEVEL_TIME_DIVISION,StudentEditPage.Form.FIELD_CLASSROOMSESSION,StudentEditPage.Form.FIELD_IMAGE
				,StudentEditPage.Form.FIELD_NAME,StudentEditPage.Form.FIELD_LAST_NAMES,StudentEditPage.Form.FIELD_BIRTH_DATE,StudentEditPage.Form.FIELD_BIRTH_LOCATION,LocationFormModel.FIELD_LOCALITY
				,StudentEditPage.Form.FIELD_NATIONALITY,StudentEditPage.Form.FIELD_SEX,StudentEditPage.Form.FIELD_LANGUAGE_COLLECTION,LanguageCollectionFormModel.FIELD_LANGUAGE_1
				,StudentEditPage.Form.FIELD_REGISTRATION_DATE,StudentEditPage.Form.FIELD_OTHER_DETAILS)
				.addControlSetListener(new StudentDetailsConfiguration.FormControlSetAdapter());
		
		getFormConfiguration(Student.class, Crud.UPDATE).addRequiredFieldNames(StudentEditPage.Form.FIELD_CODE)
		.addFieldNames(StudentEditPage.Form.FIELD_ADMISSION_LEVEL_TIME_DIVISION,StudentEditPage.Form.FIELD_IMAGE
				,StudentEditPage.Form.FIELD_NAME,StudentEditPage.Form.FIELD_LAST_NAMES,StudentEditPage.Form.FIELD_BIRTH_DATE,StudentEditPage.Form.FIELD_BIRTH_LOCATION,LocationFormModel.FIELD_LOCALITY
				,StudentEditPage.Form.FIELD_NATIONALITY,StudentEditPage.Form.FIELD_SEX,StudentEditPage.Form.FIELD_LANGUAGE_COLLECTION,LanguageCollectionFormModel.FIELD_LANGUAGE_1
				,StudentEditPage.Form.FIELD_REGISTRATION_DATE,StudentEditPage.Form.FIELD_OTHER_DETAILS)
				.addControlSetListener(new StudentDetailsConfiguration.FormControlSetAdapter());
		
		getFormConfiguration(Student.class, Crud.DELETE).addFieldNames(StudentEditPage.Form.FIELD_CODE,StudentEditPage.Form.FIELD_IMAGE,StudentEditPage.Form.FIELD_NAME
				,StudentEditPage.Form.FIELD_LAST_NAMES);
		
		registerDetailsConfiguration(StudentDetails.class, new StudentDetailsConfiguration());
	}
	
	protected void configureStudentClassroomSessionClass() {
		getFormConfiguration(StudentClassroomSession.class, Crud.CREATE).addRequiredFieldNames(StudentClassroomSessionEditPage.Form.FIELD_CLASSROOM_STUDENT
				,StudentClassroomSessionEditPage.Form.FIELD_CLASSROOM_SESSION);
		registerDetailsConfiguration(StudentClassroomSessionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						//if(data instanceof StudentClassroomSessionDetails)
							return isFieldNameIn(field,StudentClassroomSessionDetails.FIELD_STUDENT,StudentClassroomSessionDetails.FIELD_CLASSROOM_SESSION);
						
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter() {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,StudentClassroomSessionDetails.FIELD_STUDENT);
					}
				};
			}
		});
	}
	
	protected void configureStudentClassroomSessionDivisionClass() {
		getFormConfiguration(StudentClassroomSessionDivision.class, Crud.READ).addRequiredFieldNames(StudentClassroomSessionDivisionDetails.FIELD_NUMBER_OF_TIME_ABSENT
				,StudentClassroomSessionDivisionDetails.FIELD_GLOBAL_APPRECIATION,StudentClassroomSessionDivisionDetails.FIELD_CONFERENCE_REQUESTED
				,StudentClassroomSessionDivisionDetails.FIELD_STUDENT);
		registerDetailsConfiguration(StudentClassroomSessionDivisionDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						//if(data instanceof StudentClassroomSessionDetails)
							return isFieldNameIn(field,StudentClassroomSessionDetails.FIELD_STUDENT,StudentClassroomSessionDetails.FIELD_CLASSROOM_SESSION);
						
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter() {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,StudentClassroomSessionDivisionDetails.FIELD_STUDENT,StudentClassroomSessionDivisionDetails.FIELD_GLOBAL_APPRECIATION
								,StudentClassroomSessionDivisionDetails.FIELD_NUMBER_OF_TIME_ABSENT,StudentClassroomSessionDivisionDetails.FIELD_EVALUATION_AVERAGE_VALUE
								,StudentClassroomSessionDivisionDetails.FIELD_CONFERENCE_REQUESTED);
					}
				};
			}
		});
	}
	
	protected void configureStudentClassroomSessionDivisionSubjectClass() {
		getFormConfiguration(StudentClassroomSessionDivisionSubject.class, Crud.READ).addFieldNames(AbstractStudentClassroomSessionDivisionSubjectDetails.FIELD_STUDENT
				,AbstractStudentClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT);
		
		registerDetailsConfiguration(AbstractStudentClassroomSessionDivisionSubjectDetails.class, new StudentClassroomSessionDivisionSubjectDetailsConfiguration());
	}
	public static class StudentClassroomSessionDivisionSubjectDetailsConfiguration extends DetailsConfiguration implements Serializable{
		private static final long serialVersionUID = -2263239648035992476L;
		
		@SuppressWarnings("rawtypes")
		@Override
		public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
			return new DetailsConfiguration.DefaultControlSetAdapter(){ 
				private static final long serialVersionUID = 1L;
				@Override
				public Boolean build(Object data,Field field) {
					//if(data instanceof StudentClassroomSessionDetails)
						return isFieldNameIn(field,AbstractStudentClassroomSessionDivisionSubjectDetails.FIELD_STUDENT
								,AbstractStudentClassroomSessionDivisionSubjectDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT);
					
				}
			};
		}
		
		@Override
		public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,AbstractPrimefacesPage page) {
			return new DetailsConfiguration.DefaultColumnAdapter(){
				private static final long serialVersionUID = 1L;
				@Override
				public Boolean isColumn(Field field) {
					return isFieldNameIn(field,AbstractStudentClassroomSessionDivisionSubjectDetails.FIELD_STUDENT);
				}
			};
		}
		
	}
	
	protected void configureStudentResultsMetricValueDetailsClass() {
		registerDetailsConfiguration(StudentResultsMetricValueDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@Override
			public ColumnAdapter getTableColumnAdapter() {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,StudentResultsMetricValueDetails.FIELD_NAME,StudentResultsMetricValueDetails.FIELD_VALUE);
					}
				};
			}
		});
	}
	
	/**/
	
	protected void configureTeacherClass() {
		getFormConfiguration(Teacher.class, Crud.CREATE).addRequiredFieldNames(TeacherEditPage.Form.FIELD_CODE)
		.addFieldNames(TeacherEditPage.Form.FIELD_IMAGE,TeacherEditPage.Form.FIELD_NAME,TeacherEditPage.Form.FIELD_LAST_NAMES
				,TeacherEditPage.Form.FIELD_BIRTH_DATE,TeacherEditPage.Form.FIELD_BIRTH_LOCATION,TeacherEditPage.Form.FIELD_NATIONALITY,TeacherEditPage.Form.FIELD_SEX
				,TeacherEditPage.Form.FIELD_LANGUAGE_COLLECTION,TeacherEditPage.Form.FIELD_REGISTRATION_DATE)
				.addControlSetListener(new TeacherDetailsConfiguration.FormControlSetAdapter());
		
		getFormConfiguration(Teacher.class, Crud.UPDATE).addRequiredFieldNames(TeacherEditPage.Form.FIELD_CODE)
		.addFieldNames(TeacherEditPage.Form.FIELD_IMAGE,TeacherEditPage.Form.FIELD_NAME,TeacherEditPage.Form.FIELD_LAST_NAMES
				,TeacherEditPage.Form.FIELD_BIRTH_DATE,TeacherEditPage.Form.FIELD_BIRTH_LOCATION,TeacherEditPage.Form.FIELD_NATIONALITY,TeacherEditPage.Form.FIELD_SEX
				,TeacherEditPage.Form.FIELD_LANGUAGE_COLLECTION,TeacherEditPage.Form.FIELD_REGISTRATION_DATE);
		
		getFormConfiguration(Teacher.class, Crud.DELETE).addFieldNames(TeacherEditPage.Form.FIELD_CODE,TeacherEditPage.Form.FIELD_IMAGE,TeacherEditPage.Form.FIELD_NAME
				,TeacherEditPage.Form.FIELD_LAST_NAMES);
		
		registerDetailsConfiguration(TeacherDetails.class, new TeacherDetailsConfiguration());
	}
	
	/**/
	
	protected void configureEvaluationClass() {
		getFormConfiguration(Evaluation.class, Crud.CREATE).addRequiredFieldNames(EvaluationEditPage.Form.FIELD_TYPE);
		
		getFormConfiguration(Evaluation.class, Crud.READ).addFieldNames(EvaluationDetails.FIELD_CLASSROOM_SESSION,EvaluationDetails.FIELD_CLASSROOM_SESSION_DIVISION
				,EvaluationDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,EvaluationDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_TYPE,EvaluationDetails.FIELD_NAME);
		
		getFormConfiguration(Evaluation.class, Crud.UPDATE).addRequiredFieldNames(EvaluationEditPage.Form.FIELD_TYPE);
		
		getFormConfiguration(Evaluation.class, Crud.DELETE).addFieldNames(EvaluationEditPage.Form.FIELD_TYPE);
		
		registerDetailsConfiguration(EvaluationDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						return isFieldNameIn(field,EvaluationDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,EvaluationDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_TYPE
								,EvaluationDetails.FIELD_NAME);
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter(@SuppressWarnings("rawtypes") Class clazz,AbstractPrimefacesPage page) {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,EvaluationDetails.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_TYPE,EvaluationDetails.FIELD_NAME);
					}
				};
			}
		});
	}
	
	protected void configureStudentClassroomSessionDivisionSubjectEvaluationClass() {
		
		getFormConfiguration(Evaluation.class, Crud.READ).addFieldNames(StudentClassroomSessionDivisionSubjectEvaluationDetails.FIELD_NAMES
				,StudentClassroomSessionDivisionSubjectEvaluationDetails.FIELD_MARK);
		
		registerDetailsConfiguration(StudentClassroomSessionDivisionSubjectEvaluationDetails.class, new DetailsConfiguration(){
			private static final long serialVersionUID = 1L;
			@SuppressWarnings("rawtypes")
			@Override
			public ControlSetAdapter.Details getFormControlSetAdapter(Class clazz) {
				return new DetailsConfiguration.DefaultControlSetAdapter(){ 
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean build(Object data,Field field) {
						return isFieldNameIn(field,StudentClassroomSessionDivisionSubjectEvaluationDetails.FIELD_NAMES
								,StudentClassroomSessionDivisionSubjectEvaluationDetails.FIELD_MARK);
					}
				};
			}
			
			@Override
			public ColumnAdapter getTableColumnAdapter() {
				return new DetailsConfiguration.DefaultColumnAdapter(){
					private static final long serialVersionUID = 1L;
					@Override
					public Boolean isColumn(Field field) {
						return isFieldNameIn(field,StudentClassroomSessionDivisionSubjectEvaluationDetails.FIELD_NAMES
								,StudentClassroomSessionDivisionSubjectEvaluationDetails.FIELD_MARK);
					}
				};
			}
		});
	}
}
