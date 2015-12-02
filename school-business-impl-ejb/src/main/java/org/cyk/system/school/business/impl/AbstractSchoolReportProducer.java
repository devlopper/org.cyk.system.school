package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;

import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.model.NodeResults;
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

public abstract class AbstractSchoolReportProducer extends AbstractCompanyReportProducer implements SchoolReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	@Override
	public String getEvaluationTypeCode(StudentSubjectEvaluation studentSubjectEvaluation) {
		return studentSubjectEvaluation.getSubjectEvaluation().getType().getType().getCode();
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
		RootBusinessLayer.getInstance().getContactCollectionBusiness().load(as.getSchool().getOwnedCompany().getCompany().getContactCollection());
		set(as.getSchool().getOwnedCompany().getCompany().getContactCollection(), r.getAcademicSession().getCompany().getContact());
		if(cs.getCoordinator()!=null)
			r.getCommentator().getPerson().setNames(cs.getCoordinator().getPerson().getNames());
		
		r.getClassroomSessionDivision().getClassroomSession().setName(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(cs));
		
		//debug(results);
		r.getClassroomSessionDivision().setName(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(csd));
		r.getClassroomSessionDivision().setAverage(format(results.getAverage()));
		r.getClassroomSessionDivision().setHighestAverage(format(results.getAverageHighest()));
		r.getClassroomSessionDivision().setLowestAverage(format(results.getAverageLowest()));
		r.getClassroomSessionDivision().setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		r.getClassroomSessionDivision().setOpenedTime(format(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),csd.getDuration())));
		r.setAttendedTime(format(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getAttendedDuration())));
		r.setMissedTime(format(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(csd.getClassroomSession(),s.getResults().getLectureAttendance().getMissedDuration())));
		
		set(student, r.getStudent());
		
		if(cs.getCoordinator()!=null)
			set(cs.getCoordinator(), r.getCommentator());
		
		if(as.getSchool().getOwnedCompany().getCompany().getSigner()!=null)
			set(as.getSchool().getOwnedCompany().getCompany().getSigner(), r.getSigner());
		
		r.setComments(s.getResults().getAppreciation());
		r.setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
		
		//debug(s.getResults().getEvaluationSort());
		//debug(s.getResults());
		//debug(s.getResults().getEvaluationSort());
		//debug(s.getResults().getEvaluationSort().getAverageInterval());
		
		r.setAverageScale(getGradeScaleCode(s.getResults().getEvaluationSort().getAverageInterval()));
		r.setRank(RootBusinessLayer.getInstance().getMathematicsBusiness().format(s.getResults().getEvaluationSort().getRank()));
		r.setName(languageBusiness.findText("school.report.studentclassroomsessiondivision.title",new Object[]{csd.getUiString()}));
		r.setSubjectsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.subject"));
		r.setCommentsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.comments"));
		r.setSchoolStampBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.schoolstamp"));
		
		r.setTotalCoefficient(format(s.getResults().getEvaluationSort().getAverage().getDivisor()));
		r.setTotalAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
		r.setTotalAverageCoefficiented(format(s.getResults().getEvaluationSort().getAverage().getDividend()));
		
		//r.setMissedTime((s.getResults().getLectureAttendance().getMissedDuration()/DateUtils.MILLIS_PER_HOUR) +"");
		//r.setMissedTimeJustified((s.getResults().getLectureAttendance().getMissedDurationJustified()/DateUtils.MILLIS_PER_HOUR)+"");
		
		if(s.getResults().getEvaluationSort().getRank()==null)
			;
		else
			processStudentSubjects(r, s,parameters);
				
		produceStudentClassroomSessionDivisionReportLabelValueCollections(r);
			
		return r;
	}
	
	protected void processStudentSubjects(StudentClassroomSessionDivisionReport r,StudentClassroomSessionDivision s,StudentClassroomSessionDivisionReportParameters parameters){
		logTrace("Number of student subjects = {}", s.getDetails().size());
		for(StudentSubject studentSubject : s.getDetails()){
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
			if(studentSubject.getClassroomSessionDivisionSubject().getTeacher()!=null)
				set(studentSubject.getClassroomSessionDivisionSubject().getTeacher(), sr.getTeacher());
			
			sr.setAverage(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()):NOT_APPLICABLE);
			sr.setAverageCoefficiented(applicable?format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()
					.multiply(studentSubject.getClassroomSessionDivisionSubject().getCoefficient())):NOT_APPLICABLE);
			sr.setRank(applicable?rootBusinessLayer.getMathematicsBusiness().format(studentSubject.getResults().getEvaluationSort().getRank()):NOT_APPLICABLE);	
			
			if(studentSubject.getResults().getEvaluationSort().getAverageInterval()!=null){
				set(studentSubject.getResults().getEvaluationSort().getAverageInterval(), sr.getAverageScale());
				sr.getAverageScale().setCode(getGradeScaleCode(studentSubject.getResults().getEvaluationSort().getAverageInterval()));
			}
			
			BigDecimal[] results = new BigDecimal[]{BigDecimal.ZERO};
			studentSubjectEvaluation(sr, studentSubject, studentSubject.getDetails(), results,parameters);
		}
		
		if(Boolean.TRUE.equals(parameters.getSumMarks())){
			for(int i=0;i<parameters.getEvaluationTypeCodes().size();i++)
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
		if(Boolean.FALSE.equals(studentSubjectEvaluation.getSubjectEvaluation().getCoefficientApplied()))
			value = value.multiply(studentSubjectEvaluation.getSubjectEvaluation().getType().getCoefficient());
		return value;
	}
	
	protected void produceStudentClassroomSessionDivisionReportLabelValueCollections(StudentClassroomSessionDivisionReport r){
		StudentClassroomSessionDivision studentClassroomSessionDivision = (StudentClassroomSessionDivision) r.getSource();
		
		r.setStudentLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.student"));
		labelValue("school.report.studentclassroomsessiondivision.block.student.names", r.getStudent().getPerson().getNames());
		labelValue("school.report.studentclassroomsessiondivision.block.student.surname", r.getStudent().getPerson().getSurname());
		labelValue("school.report.studentclassroomsessiondivision.block.student.birthdate", r.getStudent().getPerson().getBirthDate());
		labelValue("school.report.studentclassroomsessiondivision.block.student.birthlocation", r.getStudent().getPerson().getBirthLocation());
		labelValue("school.report.studentclassroomsessiondivision.block.student.registrationcode", r.getStudent().getRegistrationCode());
		labelValue("school.report.studentclassroomsessiondivision.block.student.classroomsessionname", r.getClassroomSessionDivision().getClassroomSession().getName());
		labelValue("school.report.studentclassroomsessiondivision.block.student.sex", r.getStudent().getPerson().getSex());
		
		r.setAttendanceLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.attendance"));
		labelValue("school.report.studentclassroomsessiondivision.block.attendance.opened", r.getClassroomSessionDivision().getOpenedTime());
		labelValue("school.report.studentclassroomsessiondivision.block.attendance.present", r.getAttendedTime());
		labelValue("school.report.studentclassroomsessiondivision.block.attendance.absent", r.getMissedTime());
		
		r.setOverallResultlLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.overallresult"));
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.average", r.getAverage());
		labelValue(LABEL_VALUE_STUDENTCLASSROOMSESSIONDIVISION_BLOCK_OVERALLRESULT_GRADE_ID, r.getAverageScale());
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.rank", r.getRank());
		
		r.setGradingScaleLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.gradingscale"));
		IntervalCollection evaluationIntervalCollection = ((StudentClassroomSessionDivision)r.getSource()).getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentClassroomSessionDivisionAverageScale();
		//rootBusinessLayer.getIntervalCollectionBusiness().load(evaluationIntervalCollection);
		for(Interval interval : rootBusinessLayer.getIntervalBusiness().findByCollection(evaluationIntervalCollection, Boolean.FALSE)){
			LabelValueReport labelValueReport = new LabelValueReport(currentLabelValueCollection,null, getGradeScaleCode(interval), interval.getName());
			labelValueReport.addExtendedValues(format(interval.getLow().getValue())+" - "+format(interval.getHigh().getValue()));
			currentLabelValueCollection.getCollection().add(labelValueReport);
		}
		
		r.setEffortLevelLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.effort"));
		IntervalCollection intervalCollection = ((StudentClassroomSessionDivision)r.getSource()).getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentWorkMetricCollection().getValueIntervalCollection();
		//rootBusinessLayer.getIntervalCollectionBusiness().load(intervalCollection);
		for(Interval interval : rootBusinessLayer.getIntervalBusiness().findByCollection(intervalCollection, Boolean.TRUE)){
			LabelValueReport labelValueReport = new LabelValueReport(currentLabelValueCollection,null, interval.getCode(), interval.getName());
			labelValueReport.addExtendedValues(format(interval.getLow().getValue())+" - "+format(interval.getHigh().getValue()));
			currentLabelValueCollection.getCollection().add(labelValueReport);
		}
		
		r.setBehaviorLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.behaviour"));
		MetricCollection metricCollection = ((StudentClassroomSessionDivision)r.getSource()).getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentWorkMetricCollection();
		rootBusinessLayer.getMetricCollectionBusiness().load(metricCollection);
		for(Metric metric : metricCollection.getCollection()){
			String value = "";
			for(StudentResultsMetricValue studentResultsMetricValue : SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResults(studentClassroomSessionDivision.getResults()))
				if(studentResultsMetricValue.getMetricValue().getMetric().getIdentifier().equals(metric.getIdentifier())){
					value = format(studentResultsMetricValue.getMetricValue().getValue());
					break;
				}
			LabelValueReport labelValueReport = new LabelValueReport(currentLabelValueCollection,null, metric.getName(), value);
			currentLabelValueCollection.getCollection().add(labelValueReport);
		}
	}

	protected String getGradeScaleCode(Interval interval){
		return interval.getCode();
	}
	
	/**/
	
	public static final String LABEL_VALUE_STUDENTCLASSROOMSESSIONDIVISION_BLOCK_OVERALLRESULT_GRADE_ID = "school.report.studentclassroomsessiondivision.block.overallresult.grade";
	
}
