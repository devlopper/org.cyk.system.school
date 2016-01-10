package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Arrays;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.party.person.AbstractActor;
import org.cyk.system.root.ui.web.primefaces.api.RootWebManager;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.AbstractSubjectDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionConsultPage;
import org.cyk.ui.api.data.collector.form.FormConfiguration;
import org.cyk.ui.api.model.party.AbstractActorEditFormModel;
import org.cyk.ui.api.model.party.DefaultActorReadFormModel;
import org.cyk.ui.api.model.party.DefaultPersonEditFormModel;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormManyPageListener;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormOnePageListener;
import org.cyk.ui.web.primefaces.page.crud.AbstractActorConsultPage;
import org.cyk.ui.web.primefaces.page.crud.AbstractActorConsultPage.MainDetails;
import org.cyk.ui.web.primefaces.page.tools.AbstractActorConsultPageAdapter;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;
import org.cyk.utility.common.cdi.AbstractBean;

@WebListener
public class IesaContextListener extends AbstractSchoolContextListener implements Serializable {

	private static final long serialVersionUID = -9042005596731665575L;
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		StudentClassroomSessionDivisionConsultPage.SUBJECT_DETAILS_CLASS_NAME = SubjectDetails.class.getName();
		StudentClassroomSessionDivisionConsultPage.LOAD_EVALUATIONS = Boolean.TRUE;
		
		SchoolBusinessLayer.getInstance().setReportProducer(new ReportProducer());
		SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.getEvaluationTypeCodes().addAll(Arrays.asList("Test1","Test2","Exam"));
    	SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.setSumMarks(Boolean.TRUE);
    	StudentClassroomSessionDivisionBusiness.DEFAULT_BUILD_REPORT_OPTIONS.setAttendance(Boolean.FALSE);
	}
	
	@Override
	protected void applicationUImanagers(ServletContextEvent event) {
		super.applicationUImanagers(event);
		uiManager.registerApplicationUImanager(RootWebManager.getInstance());
		uiManager.registerApplicationUImanager(SchoolWebManager.getInstance());
	}
	
	@Override
	protected <IDENTIFIABLE extends AbstractIdentifiable> void registerBusinessEntityFormOnePageListener(Class<IDENTIFIABLE> aClass,BusinessEntityFormOnePageListener<?> listener) {
		super.registerBusinessEntityFormOnePageListener(aClass, listener);
		if(aClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(FormConfiguration.TYPE_INPUT_SET_SMALLEST).addRequiredFieldNames(AbstractActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(FormConfiguration.TYPE_INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_TITLE
					,DefaultPersonEditFormModel.FIELD_SIGNATURE_SPECIMEN);
			
			listener.getFormConfigurationMap().get(Crud.UPDATE).get(DefaultPersonEditFormModel.TAB_PERSON_ID).addFieldNames(
					AbstractActorEditFormModel.FIELD_REGISTRATION_CODE,DefaultPersonEditFormModel.FIELD_TITLE);
			
		}else if(aClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(FormConfiguration.TYPE_INPUT_SET_SMALLEST).addRequiredFieldNames(AbstractActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(FormConfiguration.TYPE_INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_SURNAME
					,DefaultPersonEditFormModel.FIELD_BIRTH_DATE,DefaultPersonEditFormModel.FIELD_BIRTH_LOCATION
					,DefaultPersonEditFormModel.FIELD_SEX,DefaultPersonEditFormModel.FIELD_IMAGE);
			
			listener.getFormConfigurationMap().get(Crud.UPDATE).get(DefaultPersonEditFormModel.TAB_PERSON_ID).addFieldNames(
					AbstractActorEditFormModel.FIELD_REGISTRATION_CODE,DefaultPersonEditFormModel.FIELD_SURNAME
					,DefaultPersonEditFormModel.FIELD_BIRTH_DATE,DefaultPersonEditFormModel.FIELD_BIRTH_LOCATION
					,DefaultPersonEditFormModel.FIELD_SEX,DefaultPersonEditFormModel.FIELD_IMAGE);
		}
	}
	
	@Override
	protected <IDENTIFIABLE extends AbstractIdentifiable> void registerBusinessEntityFormManyPageListener(Class<IDENTIFIABLE> aClass,BusinessEntityFormManyPageListener<?> listener) {
		if(aClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(FormConfiguration.TYPE_INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}else if(aClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(FormConfiguration.TYPE_INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}
		super.registerBusinessEntityFormManyPageListener(aClass, listener);
	}
	
	@Override
	protected void identifiableConfiguration(ServletContextEvent event) {
		super.identifiableConfiguration(event);
		//IdentifiableConfiguration identifiableConfiguration = new IdentifiableConfiguration(StudentClassroomSessionDivision.class, editOneFormModelClass, readOneFormModelClass);
		//uiManager.registerConfiguration(configuration);
	}
	
	/**/
	
	public static class SubjectDetails extends AbstractSubjectDetails implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FILED_SUBJECT) private String test1;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_TEST1) private String test2;
		@Input @InputText @Sequence(direction=Direction.AFTER,field=FIELD_TEST2) private String exam;
		public SubjectDetails(StudentSubject studentSubject) {
			super(studentSubject);
			for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubject.getDetails()){
				if(studentSubjectEvaluation.getStudentSubject().equals(studentSubject)){
					if(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals("Test1"))
						test1 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals("Test2"))
						test2 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode().equals("Exam"))
						exam = numberBusiness.format(studentSubjectEvaluation.getValue());
				}
					
			}
		}
		public static final String FIELD_TEST1 = "test1";
		public static final String FIELD_TEST2 = "test2";
		public static final String FIELD_EXAM = "exam";
	}
	
	public static class ReportProducer extends AbstractSchoolReportProducer{
		private static final long serialVersionUID = 246685915578107971L;
    	
		@Override
		public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
				StudentClassroomSessionDivisionReportParameters parameters) {
			StudentClassroomSessionDivisionReport r = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,parameters);
			r.getAcademicSession().getCompany().setName("<style forecolor=\"red\">I</style>NTERNATIONAL <style forecolor=\"red\">E</style>NGLISH <style forecolor=\"red\">S</style>CHOOL"
					+ " OF <style forecolor=\"red\">A</style>BIDJAN");
			
			r.getSubjectsTableColumnNames().add("No.");
			r.getSubjectsTableColumnNames().add("SUBJECTS");
			r.getSubjectsTableColumnNames().add("Test 1 15%");
			r.getSubjectsTableColumnNames().add("Test 2 15%");
			r.getSubjectsTableColumnNames().add("Exam 70%");
			r.getSubjectsTableColumnNames().add("TOTAL 100%");
			r.getSubjectsTableColumnNames().add("GRADE");
			r.getSubjectsTableColumnNames().add("RANK");
			r.getSubjectsTableColumnNames().add("OUT OF");
			r.getSubjectsTableColumnNames().add("MAX");
			r.getSubjectsTableColumnNames().add("CLASS AVERAGE");
			r.getSubjectsTableColumnNames().add("REMARKS");
			r.getSubjectsTableColumnNames().add("TEACHER");
			
			r.setInformationLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.informations"));
			if(studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()==2){
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualaverage", "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualgrade", "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualrank", "To Compute");
				//labelValue("school.report.studentclassroomsessiondivision.block.informations.promotion", 
				//		studentClassroomSessionDivision.get "To Compute");
			}else{
				labelValue("school.report.studentclassroomsessiondivision.block.informations.nextacademicsession", 
						format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNextStartingDate()));
			}
			
			r.setBehaviorLabelValueCollection1(new LabelValueCollectionReport());
			r.getBehaviorLabelValueCollection1().setName("school.report.studentclassroomsessiondivision.block.behaviour");
			for(int i=0;i<=5;i++)
				r.getBehaviorLabelValueCollection1().getCollection().add(r.getBehaviorLabelValueCollection().getCollection().get(i));
			
			r.setBehaviorLabelValueCollection2(new LabelValueCollectionReport());
			r.getBehaviorLabelValueCollection2().setName("school.report.studentclassroomsessiondivision.block.behaviour");
			for(int i=6;i<=11;i++)
				r.getBehaviorLabelValueCollection2().getCollection().add(r.getBehaviorLabelValueCollection().getCollection().get(i));
			
			return r;
		}
		
    }
	
	/**/
	
	@SuppressWarnings("unchecked")
	@Override
	protected <ACTOR extends AbstractActor> AbstractActorConsultPageAdapter<ACTOR> getActorConsultPageAdapter(Class<ACTOR> actorClass) {
		if(actorClass.equals(Student.class))
			return (AbstractActorConsultPageAdapter<ACTOR>) new StudentConsultPageAdapter();
		else if(actorClass.equals(Teacher.class))
			return (AbstractActorConsultPageAdapter<ACTOR>) new TeacherConsultPageAdapter();
		return super.getActorConsultPageAdapter(actorClass);
	}
	
	private static class TeacherConsultPageAdapter extends AbstractActorConsultPage.Adapter<Teacher>{

		private static final long serialVersionUID = -5657492205127185872L;

		public TeacherConsultPageAdapter() {
			super(Teacher.class);
		}
		
		@Override
		public void initialisationEnded(AbstractBean bean) {
			super.initialisationEnded(bean);
			((AbstractActorConsultPage<?>)bean).removeDetailsMenuCommandable(DefaultPersonEditFormModel.TAB_CONTACT_ID);
			((AbstractActorConsultPage<?>)bean).removeDetailsMenuCommandable(DefaultPersonEditFormModel.TAB_SIGNATURE_ID);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public <DETAILS> ControlSetAdapter<DETAILS> getControlSetAdapter(Class<DETAILS> detailsClass) {
			if(MainDetails.class.equals(detailsClass)){
				return (ControlSetAdapter<DETAILS>) new ControlSetAdapter<MainDetails>(){
					@Override
					public Boolean build(Field field) {
						return field.getName().equals(MainDetails.FIELD_TITLE) || field.getName().equals(MainDetails.FIELD_FIRSTNAME) 
								|| field.getName().equals(MainDetails.FIELD_LASTNAMES) || field.getName().equals(MainDetails.FIELD_REGISTRATION_CODE)
								|| field.getName().equals(MainDetails.FIELD_REGISTRATION_DATE);
					}
				};
			}
			return super.getControlSetAdapter(detailsClass);
		}
		
	}
	
	private static class StudentConsultPageAdapter extends AbstractActorConsultPage.Adapter<Student>{

		private static final long serialVersionUID = -5657492205127185872L;

		public StudentConsultPageAdapter() {
			super(Student.class);
		}
				
		@Override
		public void initialisationEnded(AbstractBean bean) {
			super.initialisationEnded(bean);
			((AbstractActorConsultPage<?>)bean).removeDetailsMenuCommandable(DefaultPersonEditFormModel.TAB_CONTACT_ID);
			((AbstractActorConsultPage<?>)bean).removeDetailsMenuCommandable(DefaultPersonEditFormModel.TAB_SIGNATURE_ID);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public <DETAILS> ControlSetAdapter<DETAILS> getControlSetAdapter(Class<DETAILS> detailsClass) {
			if(MainDetails.class.equals(detailsClass)){
				return (ControlSetAdapter<DETAILS>) new ControlSetAdapter<MainDetails>(){
					@Override
					public Boolean build(Field field) {
						return field.getName().equals(MainDetails.FIELD_BIRTHDATE) || field.getName().equals(MainDetails.FIELD_FIRSTNAME) 
								|| field.getName().equals(MainDetails.FIELD_LASTNAMES) || field.getName().equals(MainDetails.FIELD_REGISTRATION_CODE)
								|| field.getName().equals(MainDetails.FIELD_REGISTRATION_DATE) || field.getName().equals(MainDetails.FIELD_BIRTHLOCATION)
								|| field.getName().equals(MainDetails.FIELD_SEX) || field.getName().equals(MainDetails.FIELD_SURNAME);
					}
				};
			}
			return super.getControlSetAdapter(detailsClass);
		}
		
	}
	
}
