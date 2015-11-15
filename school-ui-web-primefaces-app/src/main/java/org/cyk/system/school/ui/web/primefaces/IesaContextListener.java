package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.util.Arrays;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.party.person.AbstractActor;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionConsultPage;
import org.cyk.system.school.ui.web.primefaces.session.StudentClassroomSessionDivisionConsultPage.AbstractSubjectDetails;
import org.cyk.ui.api.data.collector.form.FormConfiguration.Type;
import org.cyk.ui.api.model.party.DefaultActorEditFormModel;
import org.cyk.ui.api.model.party.DefaultActorReadFormModel;
import org.cyk.ui.api.model.party.DefaultPersonEditFormModel;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormManyPageListener;
import org.cyk.ui.web.primefaces.page.BusinessEntityFormOnePageListener;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

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
	}
	
	@Override
	protected <ACTOR extends AbstractActor> void registerBusinessEntityFormOnePageListener(Class<ACTOR> actorClass,BusinessEntityFormOnePageListener<?> listener) {
		super.registerBusinessEntityFormOnePageListener(actorClass, listener);
		if(actorClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_TITLE);
		}else if(actorClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorEditFormModel.FIELD_REGISTRATION_CODE);
			listener.getFormConfigurationMap().get(Crud.CREATE).get(Type.INPUT_SET_SMALLEST).addFieldNames(DefaultPersonEditFormModel.FIELD_SURNAME
					,DefaultPersonEditFormModel.FIELD_BIRTH_DATE,DefaultPersonEditFormModel.FIELD_BIRTH_LOCATION
					,DefaultPersonEditFormModel.FIELD_SEX,DefaultPersonEditFormModel.FIELD_IMAGE);
		}
	}
	
	@Override
	protected <ACTOR extends AbstractActor> void registerBusinessEntityFormManyPageListener(Class<ACTOR> actorClass,BusinessEntityFormManyPageListener<?> listener) {
		if(actorClass.equals(Teacher.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}else if(actorClass.equals(Student.class)){
			listener.getFormConfigurationMap().get(Crud.READ).get(Type.INPUT_SET_SMALLEST).addRequiredFieldNames(DefaultActorReadFormModel.FIELD_REGISTRATION_CODE);
		}
		super.registerBusinessEntityFormManyPageListener(actorClass, listener);
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
					if(studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode().equals("Test1"))
						test1 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode().equals("Test2"))
						test2 = numberBusiness.format(studentSubjectEvaluation.getValue());
					else if(studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode().equals("Exam"))
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
			if(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findIndex(studentClassroomSessionDivision.getClassroomSessionDivision())==3){
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
	
}
