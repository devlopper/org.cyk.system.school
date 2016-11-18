package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.geography.ContactCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.MetricDao;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
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
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;
import org.cyk.utility.common.Constant;

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
			if(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET.equals(reportTemplateCode))
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
		//debug(r.getClassroomSessionDivision());
		r.setAttendedTime(format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getAttendedDuration())));
		r.setMissedTime(format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getMissedDuration())));
		
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
	
	protected String[][] convertStudentResultsMetricValueToArray(Collection<Metric> metrics,Collection<StudentResultsMetricValue> studentResultsMetricValues){
		Collection<MetricValue> metricValues = new ArrayList<>();
		for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues)
			metricValues.add(studentResultsMetricValue.getMetricValue());
		
		/*Collection<String> m = new ArrayList<>();
		for(Metric metric : metrics)
			m.add(metric.getCode());
		//System.out.println("Convert student results metric value to array. Metrics="+StringUtils.join(m,",")+" , Values="+StringUtils.join(metricValues,",")+"");
		//logTrace("Convert student results metric value to array. Metrics={} , Values={}", StringUtils.join(metrics,","),StringUtils.join(metricValues,","));*/
		return convertToArray(metrics, metricValues);
	}
	
	protected LabelValueCollectionReport addStudentResultsLabelValueCollection(AbstractReportTemplateFile<?> report,StudentResults studentResults,String metricCollectionCode,String defaultValue){
		MetricCollection metricCollection = inject(MetricCollectionDao.class).read(metricCollectionCode);
		Collection<Metric> metrics = inject(MetricDao.class).readByCollection(metricCollection);
		Collection<StudentResultsMetricValue> studentResultsMetricValues = inject(StudentResultsMetricValueBusiness.class)
				.findByStudentResults(studentResults); 
		LabelValueCollectionReport labelValueCollectionReport =  report.addLabelValueCollection(metricCollection.getName() ,convertStudentResultsMetricValueToArray(metrics, studentResultsMetricValues));
		for(LabelValueReport labelValueReport : labelValueCollectionReport.getCollection())
			if(StringUtils.isBlank(labelValueReport.getValue()))
				labelValueReport.setValue(defaultValue);
		return labelValueCollectionReport;
	}
	
	protected LabelValueCollectionReport addStudentResultsLabelValueCollection(AbstractReportTemplateFile<?> report,StudentResults studentResults,String metricCollectionCode){
		return addStudentResultsLabelValueCollection(report, studentResults, metricCollectionCode, Constant.EMPTY_STRING);
	}
	
	protected void addStudentResultsLabelValueCollection(AbstractReportTemplateFile<?> report,StudentResults studentResults,String[][] metricCollections){
		for(String[] metricCollection : metricCollections)
			addStudentResultsLabelValueCollection(report, studentResults, metricCollection[0],metricCollection[1]);
	}
	
	protected void addStudentResultsLabelValueCollection(AbstractReportTemplateFile<?> report,StudentResults studentResults,String[] metricCollectionCodes){
		for(String metricCollectionCode : metricCollectionCodes)
			addStudentResultsLabelValueCollection(report, studentResults, metricCollectionCode);
	}
	
	/**/
	
	public static class Default extends AbstractSchoolReportProducer implements Serializable{
		private static final long serialVersionUID = 1L;
		
	}
	
	/**/
	
	public static final String LABEL_VALUE_STUDENTCLASSROOMSESSIONDIVISION_BLOCK_OVERALLRESULT_GRADE_ID = "school.report.studentclassroomsessiondivision.block.overallresult.grade";
	
}
