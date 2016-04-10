package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.utility.common.Constant;

public abstract class AbstractSchoolReportProducer extends AbstractCompanyReportProducer implements SchoolReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	private SchoolBusinessLayer schoolBusinessLayer;
	
	public AbstractSchoolReportProducer() {
		schoolBusinessLayer = SchoolBusinessLayer.getInstance();
	}
	
	@Override
	public String getEvaluationTypeCode(StudentSubjectEvaluation studentSubjectEvaluation) {
		return studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getCode();
	}
	
	@Override
	public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
			StudentClassroomSessionDivisionReportParameters parameters) {
		StudentClassroomSessionDivisionReport r = new StudentClassroomSessionDivisionReport();
		r.setSource(studentClassroomSessionDivision);
		
		Student student = studentClassroomSessionDivision.getStudent();
		StudentClassroomSessionDivision s = studentClassroomSessionDivision;
		ClassroomSessionDivision csd = s.getClassroomSessionDivision();
		ClassroomSession cs = s.getClassroomSessionDivision().getClassroomSession();
		AcademicSession as = s.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
		NodeResults results = csd.getResults();
		
		r.getAcademicSession().setFromDateToDate(timeBusiness.formatPeriodFromTo(as.getPeriod()));
		r.getAcademicSession().getCompany().setImage(RootBusinessLayer.getInstance().getFileBusiness().findInputStream(as.getSchool().getOwnedCompany().getCompany().getImage()));
		r.getAcademicSession().getCompany().setName(as.getSchool().getOwnedCompany().getCompany().getName());
		
		//debug(schoolBusinessLayer.getClassroomSessionBusiness().findCommonNodeInformations(cs));
		//debug(schoolBusinessLayer.getClassroomSessionBusiness().findCommonNodeInformations(cs).getStudentClassroomSessionDivisionResultsReportTemplate());
		
		ReportTemplate reportTemplate = schoolBusinessLayer.getClassroomSessionBusiness().findCommonNodeInformations(cs).getStudentClassroomSessionDivisionResultsReportTemplate();
		if(reportTemplate.getHeaderImage()!=null)
			r.setHeaderImage(RootBusinessLayer.getInstance().getFileBusiness().findInputStream(reportTemplate.getHeaderImage()));
		if(reportTemplate.getBackgroundImage()!=null)
			r.setBackgroundImage(RootBusinessLayer.getInstance().getFileBusiness().findInputStream(reportTemplate.getBackgroundImage()));
		RootBusinessLayer.getInstance().getContactCollectionBusiness().load(as.getSchool().getOwnedCompany().getCompany().getContactCollection());
		set(as.getSchool().getOwnedCompany().getCompany().getContactCollection(), r.getAcademicSession().getCompany().getContact());
		if(cs.getCoordinator()!=null)
			r.getCommentator().getPerson().setNames(cs.getCoordinator().getPerson().getNames());
		
		r.getClassroomSessionDivision().getClassroomSession().setName(formatUsingBusiness(cs));
		
		//debug(results);
		r.getClassroomSessionDivision().setName(formatUsingBusiness(csd));
		r.getClassroomSessionDivision().setAverage(format(results.getAverage()));
		r.getClassroomSessionDivision().setHighestAverage(format(results.getAverageHighest()));
		r.getClassroomSessionDivision().setLowestAverage(format(results.getAverageLowest()));
		r.getClassroomSessionDivision().setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		r.getClassroomSessionDivision().setOpenedTime(format(schoolBusinessLayer.getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),csd.getDuration())));
		//debug(r.getClassroomSessionDivision());
		r.setAttendedTime(format(schoolBusinessLayer.getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getAttendedDuration())));
		r.setMissedTime(format(schoolBusinessLayer.getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getMissedDuration())));
		
		set(student, r.getStudent());
		
		if(cs.getCoordinator()!=null)
			set(cs.getCoordinator(), r.getCommentator());
		
		if(as.getSchool().getOwnedCompany().getCompany().getSigner()!=null)
			set(as.getSchool().getOwnedCompany().getCompany().getSigner(), r.getSigner());
		
		r.setComments(s.getResults().getAppreciation());
		
		if(Boolean.TRUE.equals(csd.getStudentEvaluationRequired())){
			r.setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
			r.setAverageScale(rootBusinessLayer.getIntervalBusiness().findRelativeCode(s.getResults().getEvaluationSort().getAverageInterval()));
			r.setRank(RootBusinessLayer.getInstance().getMathematicsBusiness().format(s.getResults().getEvaluationSort().getRank()));
			
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
			processStudentSubjects(r, s,parameters);
				
		produceStudentClassroomSessionDivisionReportLabelValueCollections(r,parameters);
		
		return r;
	}
	
	protected void processStudentSubjects(StudentClassroomSessionDivisionReport r,StudentClassroomSessionDivision s,StudentClassroomSessionDivisionReportParameters parameters){
		//Collection<StudentSubject> studentSubjects = s.getDetails();
		Collection<StudentSubject> studentSubjects = schoolBusinessLayer.getStudentSubjectDao().readByStudentByClassroomSessionDivision(s.getStudent(), s.getClassroomSessionDivision());
		Collection<StudentSubjectEvaluation> studentSubjectEvaluations = schoolBusinessLayer.getStudentSubjectEvaluationDao().readByStudentByClassroomSessionDivision(s.getStudent(), s.getClassroomSessionDivision());
		logTrace("Number of student subjects = {}", studentSubjects.size());
		for(StudentSubject studentSubject : studentSubjects){
			Boolean applicable = studentSubject.getResults().getEvaluationSort().getAverage().getValue()!=null;
			
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubjectReport.setAverage(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getAverage()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setCoefficient(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getCoefficient()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setHighestAverage(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getAverageHighest()):NOT_APPLICABLE);
			classroomSessionDivisionSubjectReport.setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			classroomSessionDivisionSubjectReport.setNumberOfStudents(applicable?format(studentSubject.getClassroomSessionDivisionSubject().getResults().getNumberOfStudent()):NOT_APPLICABLE);
			
			StudentClassroomSessionDivisionSubjectReport sr = new StudentClassroomSessionDivisionSubjectReport(r,classroomSessionDivisionSubjectReport);
			r.getSubjects().add(sr);
			for(int i=0;i<parameters.getEvaluationTypeCodes().size();i++){
				sr.getMarks().add(NOT_APPLICABLE);
				if(Boolean.TRUE.equals(parameters.getSumMarks())){
					r.getTempMarkTotals().add(BigDecimal.ZERO);
					r.getMarkTotals().add(NOT_APPLICABLE);
				}
			}
			
			set(studentSubject.getClassroomSessionDivisionSubject().getTeacher(), sr.getTeacher());
			
			sr.setAverage(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()):NOT_APPLICABLE);
			sr.setAverageCoefficiented(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()
					.multiply(studentSubject.getClassroomSessionDivisionSubject().getCoefficient())):NOT_APPLICABLE);
			sr.setRank(applicable?rootBusinessLayer.getMathematicsBusiness().format(studentSubject.getResults().getEvaluationSort().getRank()):NOT_APPLICABLE);	
			
			//if(studentSubject.getResults().getEvaluationSort().getAverageInterval()!=null){
				set(studentSubject.getResults().getEvaluationSort().getAverageInterval(), sr.getAverageScale());
				if(applicable)
					sr.getAverageScale().setCode(rootBusinessLayer.getIntervalBusiness().findRelativeCode(studentSubject.getResults().getEvaluationSort().getAverageInterval()));
			//}else
				//sr.getAverageScale().setCode(NOT_APPLICABLE);
			
			BigDecimal[] results = new BigDecimal[]{BigDecimal.ZERO};
			studentSubjectEvaluation(sr, studentSubject, /*studentSubject.getDetails()*/studentSubjectEvaluations, results,parameters);
		}
		
		if(Boolean.TRUE.equals(parameters.getSumMarks())){
			for(int i=0;i<parameters.getEvaluationTypeCodes().size();i++)
				if(i < r.getMarkTotals().size())
					r.getMarkTotals().set(i,format(r.getTempMarkTotals().get(i)));
		}
		
		
	}
	
	protected void studentSubjectEvaluation(StudentClassroomSessionDivisionSubjectReport sr,StudentSubject studentSubject,Collection<StudentSubjectEvaluation> studentSubjectEvaluations,BigDecimal[] results
			,StudentClassroomSessionDivisionReportParameters parameters){
		int i = 0;
		for(String evaluationTypeCode : parameters.getEvaluationTypeCodes()){
			for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluations){
				if(getEvaluationTypeCode(studentSubjectEvaluation).equals(evaluationTypeCode) 
						&& studentSubjectEvaluation.getStudentSubject().getIdentifier().equals(studentSubject.getIdentifier()) 
						){
					BigDecimal value = getMarkValue(studentSubjectEvaluation);
					if(Boolean.TRUE.equals(parameters.getSumMarks()))
						sr.getStudentClassroomSessionDivision().getTempMarkTotals().set(i, sr.getStudentClassroomSessionDivision().getTempMarkTotals().get(i).add(value));
					
					sr.getMarks().set(i,format(value));
				}
			}
			i++;
		}
	}
	
	protected BigDecimal getMarkValue(StudentSubjectEvaluation studentSubjectEvaluation){
		BigDecimal value = studentSubjectEvaluation.getValue();
		if(Boolean.FALSE.equals(studentSubjectEvaluation.getEvaluation().getCoefficientApplied()))
			value = value.multiply(studentSubjectEvaluation.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getCoefficient());
		return value;
	}
	
	protected void produceStudentClassroomSessionDivisionReportLabelValueCollections(StudentClassroomSessionDivisionReport r,StudentClassroomSessionDivisionReportParameters parameters){
		
	}
	
	protected String[][] convertStudentResultsMetricValueToArray(Collection<Metric> metrics,Collection<StudentResultsMetricValue> studentResultsMetricValues){
		Collection<MetricValue> metricValues = new ArrayList<>();
		for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues)
			metricValues.add(studentResultsMetricValue.getMetricValue());
		return convertToArray(metrics, metricValues);
	}
	
	protected LabelValueCollectionReport addStudentResultsLabelValueCollection(AbstractReportTemplateFile<?> report,StudentResults studentResults,String metricCollectionCode,String defaultValue){
		MetricCollection metricCollection = rootBusinessLayer.getMetricCollectionDao().read(metricCollectionCode);
		Collection<Metric> metrics = rootBusinessLayer.getMetricDao().readByCollection(metricCollection);
		Collection<StudentResultsMetricValue> studentResultsMetricValues = SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness()
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
	
	public static final String LABEL_VALUE_STUDENTCLASSROOMSESSIONDIVISION_BLOCK_OVERALLRESULT_GRADE_ID = "school.report.studentclassroomsessiondivision.block.overallresult.grade";
	
}
