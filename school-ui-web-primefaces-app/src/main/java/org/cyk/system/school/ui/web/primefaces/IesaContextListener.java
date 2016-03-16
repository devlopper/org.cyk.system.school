package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Arrays;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments.CharacterSet;
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
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
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
	
	public static final String MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR = "BSWHG1G6";
	public static final String MERIC_COLLECTION_G7_G12_STUDENT_BEHAVIOUR = "BSWHG7G12";
	
	public static final String LEVEL_NAME_CODE_PK = "PK";
	public static final String LEVEL_NAME_CODE_K1 = "K1";
	public static final String LEVEL_NAME_CODE_K2 = "K2";
	public static final String LEVEL_NAME_CODE_K3 = "K3";
	public static final String LEVEL_NAME_CODE_G1 = "G1";
	public static final String LEVEL_NAME_CODE_G2 = "G2";
	public static final String LEVEL_NAME_CODE_G3 = "G3";
	public static final String LEVEL_NAME_CODE_G4 = "G4";
	public static final String LEVEL_NAME_CODE_G5 = "G5";
	public static final String LEVEL_NAME_CODE_G6 = "G6";
	public static final String LEVEL_NAME_CODE_G7 = "G7";
	public static final String LEVEL_NAME_CODE_G8 = "G8";
	public static final String LEVEL_NAME_CODE_G9 = "G9";
	public static final String LEVEL_NAME_CODE_G10 = "G10";
	public static final String LEVEL_NAME_CODE_G11 = "G11";
	public static final String LEVEL_NAME_CODE_G12 = "G12";
	
	public static final String REPORT_CYK_GLOBAL_RANKABLE = "CYK_GLOBAL_RANKABLE";
	
	public static final String MERIC_COLLECTION_PK_STUDENT_EXPRESSIVE_LANGUAGE = "MCPKSEL";
	public static final String MERIC_COLLECTION_PK_STUDENT_RECEPTIVE_LANGUAGE = "MCPKSRL";
	public static final String MERIC_COLLECTION_PK_STUDENT_READING_READNESS = "MCPKSRR";
	public static final String MERIC_COLLECTION_PK_STUDENT_NUMERACY_DEVELOPMENT = "MCPKSND";
	public static final String MERIC_COLLECTION_PK_STUDENT_ARTS_MUSIC = "MCPKSAM";
	public static final String MERIC_COLLECTION_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT = "MCPKSSED";
	public static final String MERIC_COLLECTION_PK_STUDENT_GROSS_MOTOR_SKILLS = "MCPKSGMS";
	public static final String MERIC_COLLECTION_PK_STUDENT_FINE_MOTOR_SKILLS = "MCPKSFMS";
	
	public static final String MERIC_COLLECTION_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING = "MCK1SELAR";
	public static final String MERIC_COLLECTION_K1_STUDENT_COMMUNICATION_SKILLS = "MCK1SCS";
	public static final String MERIC_COLLECTION_K1_STUDENT_SCIENCE = "MCK1SS";
	public static final String MERIC_COLLECTION_K1_STUDENT_SOCIAL_STUDIES = "MCK1SSS";
	public static final String MERIC_COLLECTION_K1_STUDENT_MATHEMATICS = "MCK1SM";
	public static final String MERIC_COLLECTION_K1_STUDENT_WORK_HABITS = "MCK1SWH";
	public static final String MERIC_COLLECTION_K1_STUDENT_SOCIAL_SKILLS = "MCK1SSSK";
	
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_READING_READINESS = "MCK2K3SRR";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_READING = "MCK2K3SR";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_WRITING = "MCK2K3SW";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_LISTENING_SPEAKING_VIEWING = "MCK2K3SLSV";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_ALPHABET_IDENTIFICATION = "MCK2K3SAI";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_MATHEMATICS = "MCK2K3SM";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION = "MCK2K3SSSSME";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_ART_CRAFT = "MCK2K3SAC";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_MUSIC = "MCK2K3SMM";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_PHYSICAL_EDUCATION = "MCK2K3SPE";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_WORK_BEHAVIOUR_HABITS = "MCK2K3SWBH";
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		StudentClassroomSessionDivisionConsultPage.SUBJECT_DETAILS_CLASS_NAME = SubjectDetails.class.getName();
		StudentClassroomSessionDivisionConsultPage.LOAD_EVALUATIONS = Boolean.TRUE;
		
		SchoolBusinessLayer.getInstance().setReportProducer(new ReportProducer());
		SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.getEvaluationTypeCodes().addAll(Arrays.asList("Test1","Test2","Exam"));
    	SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.setSumMarks(Boolean.TRUE);
    	StudentClassroomSessionDivisionBusiness.BuildReportArguments.ATTENDANCE = Boolean.FALSE;
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
			LabelValueCollectionReport labelValueCollectionReport;
			StudentClassroomSessionDivisionReport r = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,parameters);
			
			AcademicSession as = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
			r.getAcademicSession().setFromDateToDate(timeBusiness.findYear(as.getPeriod().getFromDate())+"/"+timeBusiness.findYear(as.getPeriod().getToDate())+" ACADEMIC SESSION");
		
			r.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
					{"Formname(s)", r.getStudent().getPerson().getLastName()}
					,{"Surname", r.getStudent().getPerson().getName()}
					,{"Date of birth", r.getStudent().getPerson().getBirthDate()}
					,{"Place of birth", r.getStudent().getPerson().getBirthLocation()}
					,{"Admission No", r.getStudent().getRegistrationCode()}
					,{"Class", r.getClassroomSessionDivision().getClassroomSession().getName()}
					,{"Gender", r.getStudent().getPerson().getSex()}
					});
			
			r.addLabelValueCollection("SCHOOL ATTENDANCE",new String[][]{
					{"Number of times school opened",r.getClassroomSessionDivision().getOpenedTime()}
					,{"Number of times present",r.getAttendedTime()}
					,{"Number of times absent",r.getMissedTime()}
					});
			
			FormatArguments formatArguments = new FormatArguments();
			formatArguments.setIsRank(Boolean.TRUE);
			formatArguments.setType(CharacterSet.LETTER);
			String name = numberBusiness.format(studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()+1, formatArguments).toUpperCase();
			r.setName(name+" TERM , "+studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getGroup().getName().toUpperCase()
					+" REPORT");
			
			String levelNameCode = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getCode();
			if(ArrayUtils.contains(new String[]{LEVEL_NAME_CODE_PK,LEVEL_NAME_CODE_K1,LEVEL_NAME_CODE_K2,LEVEL_NAME_CODE_K3},levelNameCode)){
				r.setName(r.getName()+" SHEET");
				String performanceCodeMetricCollectionCode = null;
				if(LEVEL_NAME_CODE_PK.equals(levelNameCode)){
					addStudentResultsLabelValueCollection(r, ((StudentClassroomSessionDivision)r.getSource()).getResults(), new String[][]{
							{performanceCodeMetricCollectionCode = MERIC_COLLECTION_PK_STUDENT_EXPRESSIVE_LANGUAGE,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_RECEPTIVE_LANGUAGE,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_READING_READNESS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_NUMERACY_DEVELOPMENT,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_ARTS_MUSIC,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_GROSS_MOTOR_SKILLS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_PK_STUDENT_FINE_MOTOR_SKILLS,NOT_APPLICABLE}
						});
				}else if(LEVEL_NAME_CODE_K1.equals(levelNameCode)){
					addStudentResultsLabelValueCollection(r, ((StudentClassroomSessionDivision)r.getSource()).getResults(), new String[][]{
							{performanceCodeMetricCollectionCode = MERIC_COLLECTION_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K1_STUDENT_COMMUNICATION_SKILLS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K1_STUDENT_SCIENCE,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K1_STUDENT_SOCIAL_STUDIES,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K1_STUDENT_MATHEMATICS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K1_STUDENT_WORK_HABITS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K1_STUDENT_SOCIAL_SKILLS,NOT_APPLICABLE}
						});
				}else if(LEVEL_NAME_CODE_K2.equals(levelNameCode) || LEVEL_NAME_CODE_K3.equals(levelNameCode)){
					addStudentResultsLabelValueCollection(r, ((StudentClassroomSessionDivision)r.getSource()).getResults(), new String[][]{
							{performanceCodeMetricCollectionCode = MERIC_COLLECTION_K2_K3_STUDENT_READING_READINESS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_READING,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_WRITING,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_LISTENING_SPEAKING_VIEWING,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_ALPHABET_IDENTIFICATION,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_MATHEMATICS,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_ART_CRAFT,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_MUSIC,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_PHYSICAL_EDUCATION,NOT_APPLICABLE}
							, {MERIC_COLLECTION_K2_K3_STUDENT_WORK_BEHAVIOUR_HABITS,NOT_APPLICABLE}
						});
				}
				
				labelValueCollectionReport = addIntervalCollectionLabelValueCollection(r,rootBusinessLayer.getMetricCollectionDao().read(performanceCodeMetricCollectionCode).getValueIntervalCollection()
						,Boolean.TRUE,Boolean.FALSE,null);
				labelValueCollectionReport.add("NA", "Not Assessed");
			}else{
				String studentBehaviourMetricCollectionCode = null;
				r.setName(r.getName()+" CARD");
				String testCoef = null,examCoef = "";	
				if(ArrayUtils.contains(new String[]{LEVEL_NAME_CODE_G1,LEVEL_NAME_CODE_G2,LEVEL_NAME_CODE_G3},levelNameCode)){
					name += " LOWER";
					testCoef = "15";
					examCoef = "70";
					studentBehaviourMetricCollectionCode = MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR;
				}else if(ArrayUtils.contains(new String[]{LEVEL_NAME_CODE_G4,LEVEL_NAME_CODE_G5,LEVEL_NAME_CODE_G6},levelNameCode)){
					name += " UPPER";
					testCoef = "15";
					examCoef = "70";
					studentBehaviourMetricCollectionCode = MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR;
				}else if(ArrayUtils.contains(new String[]{LEVEL_NAME_CODE_G7,LEVEL_NAME_CODE_G8,LEVEL_NAME_CODE_G9},levelNameCode)){
					name += " JUNIOR HIGH SCHOOL";
					testCoef = "20";
					examCoef = "60";
					studentBehaviourMetricCollectionCode = MERIC_COLLECTION_G7_G12_STUDENT_BEHAVIOUR;
				}else if(ArrayUtils.contains(new String[]{LEVEL_NAME_CODE_G10,LEVEL_NAME_CODE_G11,LEVEL_NAME_CODE_G12},levelNameCode)){
					name += " SENIOR HIGH SCHOOL";
					testCoef = "20";
					examCoef = "60";
					studentBehaviourMetricCollectionCode = MERIC_COLLECTION_G7_G12_STUDENT_BEHAVIOUR;
				}
				
				r.addSubjectsTableColumnNames("No.","SUBJECTS","Test 1 "+testCoef+"%","Test 2 "+testCoef+"%","Exam "+examCoef+"%","TOTAL 100%","GRADE","RANK","OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
				
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName("OVERALL RESULT");
				labelValueCollectionReport.add("AVERAGE",r.getAverage());
				labelValueCollectionReport.add("GRADE",r.getAverageScale());
				if(Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentRankable()))
					labelValueCollectionReport.add("RANK",r.getRank());
				r.addLabelValueCollection(labelValueCollectionReport);
				
				addStudentResultsLabelValueCollection(r, ((StudentClassroomSessionDivision)r.getSource()).getResults(), studentBehaviourMetricCollectionCode);
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName(r.getCurrentLabelValueCollection().getName());
				labelValueCollectionReport.setCollection(r.getCurrentLabelValueCollection().getCollection().subList(6, 12));
				r.getCurrentLabelValueCollection().setCollection(r.getCurrentLabelValueCollection().getCollection().subList(0, 6));
				
				r.addLabelValueCollection(labelValueCollectionReport);
				
				addIntervalCollectionLabelValueCollection(r,SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findCommonNodeInformations(
					((StudentClassroomSessionDivision)r.getSource()).getClassroomSessionDivision().getClassroomSession()).getStudentClassroomSessionDivisionAverageScale()
					,Boolean.FALSE,Boolean.TRUE,new Integer[][]{{1,2}});
				
				addIntervalCollectionLabelValueCollection(r,rootBusinessLayer.getMetricCollectionDao().read(MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR).getValueIntervalCollection()
						,Boolean.TRUE,Boolean.FALSE,null);
				
			}
			
			if(studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()==2){
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualaverage", "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualgrade", "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualrank", "To Compute");
				//labelValue("school.report.studentclassroomsessiondivision.block.informations.promotion", 
				//		studentClassroomSessionDivision.get "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.nextacademicsession", 
						format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNextStartingDate()));
			}else{
				ClassroomSessionDivision nextClassroomSessionDivision = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionDao()
						.readByClassroomSessionByIndex(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()
								,new Byte((byte) (studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()+1)));
			
				r.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{
					{"CONFERENCE REQUESTED",studentClassroomSessionDivision.getResults().getConferenceRequested()==null?"NO"
							:studentClassroomSessionDivision.getResults().getConferenceRequested()?"YES":"NO"}
					,{"NEXT OPENING",format(nextClassroomSessionDivision.getPeriod().getFromDate())}
					,{"NEXT TERM EXAMINATION",format(nextClassroomSessionDivision.getPeriod().getToDate())}
					});
			}
		
			
			
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
