package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.geography.ContactCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments.CharacterSet;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.persistence.api.mathematics.IntervalCollectionDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.StudentReportTemplateFile;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;
import org.joda.time.DateTimeConstants;

public abstract class AbstractSchoolReportProducer extends AbstractCompanyReportProducer implements SchoolReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	@Override
	public Class<?> getReportTemplateFileClass(AbstractIdentifiable identifiable, String reportTemplateCode) {
		if(identifiable instanceof Student){
			if(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE.equals(reportTemplateCode))
				return StudentReportTemplateFile.class;
			if(SchoolConstant.REPORT_STUDENT_TUITION_CERTIFICATE.equals(reportTemplateCode))
				return StudentReportTemplateFile.class;
		}
		
		if(identifiable instanceof StudentClassroomSessionDivision){
			//if(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET.equals(reportTemplateCode))
				return StudentClassroomSessionDivisionReportTemplateFile.class;
		}
		
		return super.getReportTemplateFileClass(identifiable, reportTemplateCode);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public <REPORT extends AbstractReportTemplateFile<REPORT>> REPORT produce(Class<REPORT> reportClass, CreateReportFileArguments<?> createReportFileArguments) {
		if(StudentReportTemplateFile.class.equals(reportClass)){
			if(createReportFileArguments.getIdentifiable() instanceof Student)
				return (REPORT) produceStudentReport((Student)createReportFileArguments.getIdentifiable());
		}
		
		if(StudentClassroomSessionDivisionReportTemplateFile.class.equals(reportClass)){
			if(createReportFileArguments.getIdentifiable() instanceof StudentClassroomSessionDivision){
				return (REPORT) produceStudentClassroomSessionDivisionReport((StudentClassroomSessionDivision)createReportFileArguments.getIdentifiable()
						,(CreateReportFileArguments<StudentClassroomSessionDivision>) createReportFileArguments);
			}
		}
		
		return super.produce(reportClass, createReportFileArguments);
	}
	
	private StudentReportTemplateFile produceStudentReport(Student student) {
		StudentReportTemplateFile report = new StudentReportTemplateFile();
		set(student, report.getActor());
		return report;
	}
	
	@Override
	public String getEvaluationTypeCode(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation) {
		return studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode();
	}
	
	@Override
	public StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision
			,CreateReportFileArguments<StudentClassroomSessionDivision> createReportFileArguments) {
		StudentClassroomSessionDivisionReportTemplateFile r = new StudentClassroomSessionDivisionReportTemplateFile();
		r.setSource(studentClassroomSessionDivision);
		
		Student student = studentClassroomSessionDivision.getStudent();
		StudentClassroomSessionDivision s = studentClassroomSessionDivision;
		ClassroomSessionDivision csd = s.getClassroomSessionDivision();
		ClassroomSession cs = s.getClassroomSessionDivision().getClassroomSession();
		AcademicSession as = s.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
		NodeResults results = csd.getResults();
		
		r.getAcademicSession().setFromDateToDate(timeBusiness.formatPeriodFromTo(as.getExistencePeriod()));
		//r.getAcademicSession().getCompany().setImage(inject(FileBusiness.class).findInputStream(as.getSchool().getOwnedCompany().getCompany().getImage()));
		//r.getAcademicSession().getCompany().setName(as.getSchool().getOwnedCompany().getCompany().getName());
		
		//debug(inject(ClassroomSessionBusiness.class).findCommonNodeInformations(cs));
		//debug(inject(ClassroomSessionBusiness.class).findCommonNodeInformations(cs).getStudentClassroomSessionDivisionResultsReportTemplate());
		
		ReportTemplate reportTemplate = inject(ClassroomSessionBusiness.class).findCommonNodeInformations(cs).getStudentClassroomSessionDivisionResultsReportTemplate();
		if(reportTemplate.getHeaderImage()!=null)
			r.setHeaderImage(inject(FileBusiness.class).findInputStream(reportTemplate.getHeaderImage()));
		File backgroundImageFile = createReportFileArguments.getBackgroundImageFile();
		if(backgroundImageFile!=null)
			r.setBackgroundImage(inject(FileBusiness.class).findInputStream(backgroundImageFile));
		inject(ContactCollectionBusiness.class).load(as.getSchool().getOwnedCompany().getCompany().getContactCollection());
		set(as.getSchool().getOwnedCompany().getCompany().getContactCollection(), r.getAcademicSession().getCompany().getContactCollection());
		if(cs.getCoordinator()!=null)
			r.getCommentator().getPerson().setNames(cs.getCoordinator().getPerson().getNames());
		
		r.getClassroomSessionDivision().getClassroomSession().setName(formatUsingBusiness(cs));
		
		//debug(results);
		r.getClassroomSessionDivision().setName(formatUsingBusiness(csd));
		r.getClassroomSessionDivision().setAverage(format(results.getAverage()));
		r.getClassroomSessionDivision().setHighestAverage(format(results.getAverageHighest()));
		r.getClassroomSessionDivision().setLowestAverage(format(results.getAverageLowest()));
		r.getClassroomSessionDivision().setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		r.getClassroomSessionDivision().setOpenedTime(format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),csd.getExistencePeriod().getNumberOfMillisecond().getSystemAs(Long.class))));
		
		/*
		//debug(r.getClassroomSessionDivision());
		r.setAttendedTime(format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getAttendedDuration())));
		r.setMissedTime(format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getMissedDuration())));
		*/
		set(student, r.getStudent());
		
		if(cs.getCoordinator()!=null)
			set(cs.getCoordinator(), r.getCommentator());
		
		if(as.getSchool().getOwnedCompany().getCompany().getSigner()!=null)
			set(as.getSchool().getOwnedCompany().getCompany().getSigner(), r.getSigner());
		
		r.setComments(s.getResults().getAppreciation());
		
		if(Boolean.TRUE.equals(csd.getStudentEvaluationRequired())){
			r.setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
			r.setAverageScale(inject(IntervalBusiness.class).findRelativeCode(s.getResults().getEvaluationSort().getAverageAppreciatedInterval()));
			r.setRank(inject(MathematicsBusiness.class).format(s.getResults().getEvaluationSort().getRank()));
			r.setAveragePromotionScale(inject(IntervalBusiness.class).findRelativeCode(s.getResults().getEvaluationSort().getAveragePromotedInterval()));
			
			r.setTotalCoefficient(format(s.getResults().getEvaluationSort().getAverage().getDivisor()));
			r.setTotalAverage(format(s.getResults().getEvaluationSort().getAverage().getDividend()));
			r.setTotalAverageCoefficiented(format(s.getResults().getEvaluationSort().getAverage().getDividend()));
		}
		
		//debug(s.getResults().getEvaluationSort());
		//debug(s.getResults());
		//debug(s.getResults().getEvaluationSort());
		//debug(s.getResults().getEvaluationSort().getAverageInterval());
			
		
		r.setName(languageBusiness.findText("school.report.studentclassroomsessiondivision.title",new Object[]{csd.getUiString()}));
		r.setSubjectsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.subject"));
		r.setCommentsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.comments"));
		r.setSchoolStampBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.schoolstamp"));
		
		//r.setMissedTime((s.getResults().getLectureAttendance().getMissedDuration()/DateUtils.MILLIS_PER_HOUR) +"");
		//r.setMissedTimeJustified((s.getResults().getLectureAttendance().getMissedDurationJustified()/DateUtils.MILLIS_PER_HOUR)+"");
		
		if(s.getResults().getEvaluationSort().getRank()==null)
			;
		else
			processStudentSubjects(r, s,createReportFileArguments);
				
		produceStudentClassroomSessionDivisionReportLabelValueCollections(r,createReportFileArguments);
		
		return r;
	}
	
	protected void processStudentSubjects(StudentClassroomSessionDivisionReportTemplateFile r,StudentClassroomSessionDivision s,CreateReportFileArguments<?> arguments){
		//Collection<StudentSubject> studentSubjects = s.getDetails();
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentByClassroomSessionDivision(s.getStudent(), s.getClassroomSessionDivision());
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations = inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).readByStudentByClassroomSessionDivision(s.getStudent(), s.getClassroomSessionDivision());
		logTrace("Number of student subjects = {}", studentSubjects.size());
		for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
			Boolean applicable = studentSubject.getResults().getEvaluationSort().getAverage().getValue()!=null;
			
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubjectReport.setAverage(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getAverage()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setCoefficient(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getWeight()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setHighestAverage(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getAverageHighest()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			classroomSessionDivisionSubjectReport.setNumberOfStudents(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getNumberOfStudent()):NOT_APPLICABLE);
			
			StudentClassroomSessionDivisionSubjectReport sr = new StudentClassroomSessionDivisionSubjectReport(r,classroomSessionDivisionSubjectReport);
			r.getSubjects().add(sr);
			for(int i=0;i<StudentClassroomSessionDivisionBusiness.EVALUATION_TYPE_CODES.size();i++){
				sr.getMarks().add(NOT_APPLICABLE);
				if(Boolean.TRUE.equals(StudentClassroomSessionDivisionBusiness.SUM_MARKS[0])){
					r.getTempMarkTotals().add(BigDecimal.ZERO);
					r.getMarkTotals().add(NOT_APPLICABLE);
				}
			}
			
			set(studentSubject.getClassroomSessionDivisionSubject().getTeacher(), sr.getTeacher());
			
			sr.setAverage(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()):NOT_APPLICABLE);
			sr.setAverageCoefficiented(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()
					.multiply(studentSubject.getClassroomSessionDivisionSubject().getWeight())):NOT_APPLICABLE);
			sr.setRank(applicable?inject(MathematicsBusiness.class).format(studentSubject.getResults().getEvaluationSort().getRank()):NOT_APPLICABLE);	
			
			//if(studentSubject.getResults().getEvaluationSort().getAverageInterval()!=null){
				set(studentSubject.getResults().getEvaluationSort().getAverageAppreciatedInterval(), sr.getAverageScale());
				if(applicable)
					sr.getAverageScale().getGlobalIdentifier().setCode(inject(IntervalBusiness.class).findRelativeCode(studentSubject.getResults().getEvaluationSort().getAverageAppreciatedInterval()));
			//}else
				//sr.getAverageScale().setCode(NOT_APPLICABLE);
			
			BigDecimal[] results = new BigDecimal[]{BigDecimal.ZERO};
			studentSubjectEvaluation(sr, studentSubject, /*studentSubject.getDetails()*/studentSubjectEvaluations, results,arguments);
		}
		
		if(Boolean.TRUE.equals(StudentClassroomSessionDivisionBusiness.SUM_MARKS[0])){
			for(int i=0;i<StudentClassroomSessionDivisionBusiness.EVALUATION_TYPE_CODES.size();i++)
				if(i < r.getMarkTotals().size())
					r.getMarkTotals().set(i,format(r.getTempMarkTotals().get(i)));
		}
		
		
	}
	
	protected void studentSubjectEvaluation(StudentClassroomSessionDivisionSubjectReport sr,StudentClassroomSessionDivisionSubject studentSubject,Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations,BigDecimal[] results
			,CreateReportFileArguments<?> arguments){
		int i = 0;
		for(String evaluationTypeCode : StudentClassroomSessionDivisionBusiness.EVALUATION_TYPE_CODES){
			for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluations){
				if(getEvaluationTypeCode(studentSubjectEvaluation).equals(evaluationTypeCode) 
						&& studentSubjectEvaluation.getStudentSubject().getIdentifier().equals(studentSubject.getIdentifier()) 
						){
					BigDecimal value = getMarkValue(studentSubjectEvaluation);
					if(Boolean.TRUE.equals(StudentClassroomSessionDivisionBusiness.SUM_MARKS[0]))
						sr.getStudentClassroomSessionDivision().getTempMarkTotals().set(i, sr.getStudentClassroomSessionDivision().getTempMarkTotals().get(i).add(value));
					
					sr.getMarks().set(i,format(value));
				}
			}
			i++;
		}
	}
	
	protected BigDecimal getMarkValue(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation){
		BigDecimal value = studentSubjectEvaluation.getValue();
		if(Boolean.FALSE.equals(studentSubjectEvaluation.getEvaluation().getCoefficientApplied()))
			value = value.multiply(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getWeight());
		return value;
	}
	
	protected void produceStudentClassroomSessionDivisionReportLabelValueCollections(StudentClassroomSessionDivisionReportTemplateFile r,CreateReportFileArguments<?> arguments){
		
	}
		
	/**/
	
	public static interface Listener extends AbstractCompanyReportProducer.Listener {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		public static class Adapter extends AbstractCompanyReportProducer.Listener.Adapter implements Listener,Serializable {
			private static final long serialVersionUID = 1L;
			
			/**/
			
			public static class Default extends Listener.Adapter implements Serializable {
				private static final long serialVersionUID = 1L;
				
			}
			
		}
	}
	
	/**/
	
	public static class Default extends AbstractSchoolReportProducer implements Serializable{
		private static final long serialVersionUID = 1L;
		
		@Override
		public StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
				CreateReportFileArguments<StudentClassroomSessionDivision> arguments) {
			LabelValueCollectionReport labelValueCollectionReport;
			StudentClassroomSessionDivisionReportTemplateFile report = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,arguments);
			
			AcademicSession as = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
			report.getAcademicSession().setFromDateToDate(timeBusiness.findYear(as.getBirthDate())+"/"+timeBusiness.findYear(as.getDeathDate())+" ACADEMIC SESSION");
		
			String levelNameCode = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getLevelName().getCode();
			String effortLevelsIntervalCollectionCode = null,schoolCommunicationMetricCollectionCode=null;
			addPupilsDetails(report);
			addAttednanceDetails(report, studentClassroomSessionDivision, StringUtils.startsWith(levelNameCode, "G") ? 
					SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT : SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT);
			
			FormatArguments formatArguments = new FormatArguments();
			formatArguments.setIsRank(Boolean.TRUE);
			formatArguments.setType(CharacterSet.LETTER);
			String nameFormat = numberBusiness.format(studentClassroomSessionDivision.getClassroomSessionDivision().getOrderNumber(), formatArguments).toUpperCase();
			nameFormat += " TERM , %s REPORT %s";
		
			if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.PK,SchoolConstant.Code.LevelName.K1,SchoolConstant.Code.LevelName.K2
					,SchoolConstant.Code.LevelName.K3},levelNameCode)){
				report.setName(String.format(nameFormat, studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel()
						.getLevelName().getName().toUpperCase(),"SHEET"));
				schoolCommunicationMetricCollectionCode = SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT;
				if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.PK,SchoolConstant.Code.LevelName.K1},levelNameCode)){
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT;
					if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.PK},levelNameCode)){
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS);
						effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT;
					}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.K1},levelNameCode)){
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS);
						effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT;
					}
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.K2,SchoolConstant.Code.LevelName.K3},levelNameCode)){
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT;
					if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.K2,SchoolConstant.Code.LevelName.K3},levelNameCode)){
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION);
						addMetricsLabelValueCollection(report, studentClassroomSessionDivision,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS);
						
					}
				}
			}else{
				String studentBehaviourMetricCollectionCode = null;
				report.setName(String.format(nameFormat, studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel()
						.getLevelName().getName(),"CARD"));
				//r.setSubjectsBlockTitle("COGNITIVE ASSESSMENT");
				String testCoef = null,examCoef = "";	
				if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G1,SchoolConstant.Code.LevelName.G2,SchoolConstant.Code.LevelName.G3},levelNameCode)){
					report.setName(String.format(nameFormat, "LOWER PRIMARY","CARD"));
					testCoef = "15";
					examCoef = "70";
					studentBehaviourMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_PRIMARY_STUDENT;
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G4,SchoolConstant.Code.LevelName.G5,SchoolConstant.Code.LevelName.G6},levelNameCode)){
					report.setName(String.format(nameFormat, "UPPER PRIMARY","CARD"));
					testCoef = "15";
					examCoef = "70";
					studentBehaviourMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_PRIMARY_STUDENT;
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G7,SchoolConstant.Code.LevelName.G8,SchoolConstant.Code.LevelName.G9},levelNameCode)){
					report.setName(String.format(nameFormat, "JUNIOR HIGH SCHOOL","CARD"));
					testCoef = "20";
					examCoef = "60";
					studentBehaviourMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_SECONDARY_STUDENT;
				}else if(ArrayUtils.contains(new String[]{SchoolConstant.Code.LevelName.G10,SchoolConstant.Code.LevelName.G11,SchoolConstant.Code.LevelName.G12},levelNameCode)){
					report.setName(String.format(nameFormat, "SENIOR HIGH SCHOOL","CARD"));
					testCoef = "20";
					examCoef = "60";
					studentBehaviourMetricCollectionCode = SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT;
					effortLevelsIntervalCollectionCode = SchoolConstant.Code.IntervalCollection.BEHAVIOUR_SECONDARY_STUDENT;
				}
				schoolCommunicationMetricCollectionCode = SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT;
				report.addSubjectsTableColumnNames("No.","SUBJECTS","TEST 1 "+testCoef+"%","TEST 2 "+testCoef+"%","EXAM "+examCoef+"%","TOTAL 100%","GRADE","RANK","OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
				
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName("OVERALL RESULT");
				labelValueCollectionReport.add("AVERAGE",report.getAverage());
				labelValueCollectionReport.add("GRADE",report.getAverageScale());
				if(Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentRankable()))
					labelValueCollectionReport.add("RANK",report.getRank());
				report.addLabelValueCollection(labelValueCollectionReport);
				addMetricsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()), studentBehaviourMetricCollectionCode);
				report.getCurrentLabelValueCollection().setName(StringUtils.upperCase(report.getCurrentLabelValueCollection().getName()));
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName(report.getCurrentLabelValueCollection().getName());
				labelValueCollectionReport.setCollection(report.getCurrentLabelValueCollection().getCollection().subList(7, 14));
				report.getCurrentLabelValueCollection().setCollection(report.getCurrentLabelValueCollection().getCollection().subList(0, 7));
				
				report.addLabelValueCollection(labelValueCollectionReport);
				
				addIntervalCollectionLabelValueCollection(report,inject(ClassroomSessionBusiness.class).findCommonNodeInformations(
					((StudentClassroomSessionDivision)report.getSource()).getClassroomSessionDivision().getClassroomSession()).getStudentClassroomSessionDivisionAverageScale()
					,null,Boolean.FALSE,Boolean.TRUE,new Integer[][]{{1,2}});
				report.getCurrentLabelValueCollection().setName(StringUtils.upperCase(report.getCurrentLabelValueCollection().getName()));		
			}
			
			addIntervalCollectionLabelValueCollection(report,inject(IntervalCollectionDao.class).read(effortLevelsIntervalCollectionCode),null,Boolean.TRUE,Boolean.FALSE,null);
			addSchoolCommunications(report, studentClassroomSessionDivision,schoolCommunicationMetricCollectionCode);
			return report;
		}
		
		protected void addPupilsDetails(StudentClassroomSessionDivisionReportTemplateFile report){
			report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
				{"Formname(s)", report.getStudent().getPerson().getNames()}
				,{"Surname", report.getStudent().getPerson().getGlobalIdentifier().getName()}
				,{"Date of birth", report.getStudent().getPerson().getGlobalIdentifier().getExistencePeriod().getFrom()}
				,{"Place of birth", report.getStudent().getPerson().getGlobalIdentifier().getBirthLocation()}
				,{"Admission No", report.getStudent().getGlobalIdentifier().getCode()}
				,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
				,{"Gender", report.getStudent().getPerson().getSex()}
				});
		}
		
		protected void addAttednanceDetails(StudentClassroomSessionDivisionReportTemplateFile report,StudentClassroomSessionDivision studentClassroomSessionDivision
				,String metricCollectionCode){
			addMetricsLabelValueCollection(report, studentClassroomSessionDivision,metricCollectionCode);
			report.getCurrentLabelValueCollection().getCollection().add(0, new LabelValueReport(report.getCurrentLabelValueCollection(),null,"Number of time school opened"
					, (studentClassroomSessionDivision.getClassroomSessionDivision().getExistencePeriod().getNumberOfMillisecond().get().longValue()
							/DateTimeConstants.MILLIS_PER_DAY)+""));
		}
		
		protected void addSchoolCommunications(StudentClassroomSessionDivisionReportTemplateFile report,StudentClassroomSessionDivision studentClassroomSessionDivision,String metricCollectionCode){
			LabelValueCollectionReport labelValueCollectionReport = addMetricsLabelValueCollection(report, studentClassroomSessionDivision,metricCollectionCode);
			if(studentClassroomSessionDivision.getClassroomSessionDivision().getOrderNumber()==inject(ClassroomSessionBusiness.class)
					.findCommonNodeInformations(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()).getClassroomSessionDivisionOrderNumberInterval().getHigh().getValue().intValue()){
				StudentResults classroomSessionResults = inject(StudentClassroomSessionDao.class)
						.readByStudentByClassroomSession(studentClassroomSessionDivision.getStudent(), studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()).getResults();
				
				labelValueCollectionReport.add("ANNUAL AVERAGE",format(classroomSessionResults.getEvaluationSort().getAverage().getValue()));
				labelValueCollectionReport.add("ANNUAL GRADE"
						,classroomSessionResults.getEvaluationSort().getAverageAppreciatedInterval()==null?NULL_VALUE:inject(IntervalBusiness.class)
								.findRelativeCode(classroomSessionResults.getEvaluationSort().getAverageAppreciatedInterval()));
				labelValueCollectionReport.add("ANNUAL RANK",inject(MathematicsBusiness.class).format(classroomSessionResults.getEvaluationSort().getRank()));
				labelValueCollectionReport.add("PROMOTION INFORMATION",
						classroomSessionResults.getEvaluationSort().getAveragePromotedInterval()==null?NULL_VALUE:classroomSessionResults.getEvaluationSort()
								.getAveragePromotedInterval().getName().toUpperCase());
				labelValueCollectionReport.add("NEXT ACADEMIC SESSION",format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()
						.getAcademicSession().getNextStartingDate()));
				
			}else{
				ClassroomSessionDivision nextClassroomSessionDivision = inject(ClassroomSessionDivisionDao.class)
						.readByClassroomSessionByOrderNumber(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()
								,(studentClassroomSessionDivision.getClassroomSessionDivision().getOrderNumber()));
			
				labelValueCollectionReport.add("NEXT OPENING",format(nextClassroomSessionDivision.getBirthDate()));
				labelValueCollectionReport.add("NEXT TERM EXAMINATION",format(nextClassroomSessionDivision.getDeathDate()));
			}
		}
		
	}
	
	/**/
	
	public static final String LABEL_VALUE_STUDENTCLASSROOMSESSIONDIVISION_BLOCK_OVERALLRESULT_GRADE_ID = "school.report.studentclassroomsessiondivision.block.overallresult.grade";
	
}
