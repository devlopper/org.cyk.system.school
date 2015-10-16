package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.file.report.LabelValueReport;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.model.NodeResults;
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

	@Override
	public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
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
		
		r.getCommentator().getPerson().setNames(cs.getCoordinator().getPerson().getNames());
		
		r.getClassroomSessionDivision().getClassroomSession().setName(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getUiString());
		
		r.getClassroomSessionDivision().setName(cs.getUiString());
		r.getClassroomSessionDivision().setAverage(format(results.getAverage()));
		r.getClassroomSessionDivision().setHighestAverage(format(results.getAverageHighest()));
		r.getClassroomSessionDivision().setLowestAverage(format(results.getAverageLowest()));
		r.getClassroomSessionDivision().setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		
		set(student, r.getStudent());
		
		set(cs.getCoordinator(), r.getCommentator());
		set(as.getSchool().getOwnedCompany().getCompany().getManager(), r.getSigner());
		
		r.setComments(s.getResults().getAppreciation());
		r.setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
		
		r.setAverageScale(s.getResults().getEvaluationSort().getAverageInterval().getCode());
		r.setRank(RootBusinessLayer.getInstance().getMathematicsBusiness().format(s.getResults().getEvaluationSort().getRank()));
		r.setName(languageBusiness.findText("school.report.studentclassroomsessiondivision.title",new Object[]{csd.getUiString()}));
		r.setSubjectsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.subject"));
		r.setCommentsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.comments"));
		r.setSchoolStampBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.schoolstamp"));
		
		r.setTotalCoefficient(format(s.getResults().getEvaluationSort().getAverage().getDivisor()));
		r.setTotalAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
		r.setTotalAverageCoefficiented(format(s.getResults().getEvaluationSort().getAverage().getDividend()));
		
		r.setMissedTime((s.getResults().getLectureAttendance().getMissedDuration()/DateUtils.MILLIS_PER_HOUR) +"");
		r.setMissedTimeJustified((s.getResults().getLectureAttendance().getMissedDurationJustified()/DateUtils.MILLIS_PER_HOUR)+"");
		
		processStudentSubjects(r, s);
				
		produceStudentClassroomSessionDivisionReportLabelValueCollections(r);
			
		return r;
	}
	
	protected void processStudentSubjects(StudentClassroomSessionDivisionReport r,StudentClassroomSessionDivision s){
		for(StudentSubject studentSubject : s.getDetails()){
			ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubjectReport = new ClassroomSessionDivisionSubjectReport();
			classroomSessionDivisionSubjectReport.setAverage("???");
			classroomSessionDivisionSubjectReport.setCoefficient(RootBusinessLayer.getInstance().getNumberBusiness()
					.format(studentSubject.getClassroomSessionDivisionSubject().getCoefficient()));
			classroomSessionDivisionSubjectReport.setHighestAverage("???");
			classroomSessionDivisionSubjectReport.setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			classroomSessionDivisionSubjectReport.setNumberOfStudents("???");
			
			StudentClassroomSessionDivisionSubjectReport sr = new StudentClassroomSessionDivisionSubjectReport(r,classroomSessionDivisionSubjectReport);
			sr.setAverage(format(studentSubject.getResults().getEvaluationSort().getAverage().getValue()));
			sr.setAverageCoefficiented(format(studentSubject.getResults().getEvaluationSort().getAverage().getValue().multiply(studentSubject.getClassroomSessionDivisionSubject().getCoefficient())));
			set(studentSubject.getResults().getEvaluationSort().getAverageInterval(), sr.getAverageScale());
			sr.setRank(rootBusinessLayer.getMathematicsBusiness().format(studentSubject.getResults().getEvaluationSort().getRank()));
			set(studentSubject.getClassroomSessionDivisionSubject().getTeacher(), sr.getTeacher());
			BigDecimal studentSubjectEvaluationMarkValueTotal = BigDecimal.ZERO;
			for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubject.getDetails()){
				BigDecimal value = getMarkValue(studentSubjectEvaluation);
				sr.getMarks().add(format(value));
				markAdded(studentSubject, studentSubjectEvaluation, value);
				studentSubjectEvaluationMarkValueTotal = studentSubjectEvaluationMarkValueTotal.add(value);
			}
			//r.getMarkTotals().add(format(studentSubjectEvaluationMarkValueTotal));
			r.getSubjects().add(sr);
		}
	}
	
	protected BigDecimal getMarkValue(StudentSubjectEvaluation studentSubjectEvaluation){
		BigDecimal value = studentSubjectEvaluation.getValue();
		if(Boolean.FALSE.equals(studentSubjectEvaluation.getSubjectEvaluation().getCoefficientApplied()))
			value = value.multiply(studentSubjectEvaluation.getSubjectEvaluation().getType().getCoefficient());
		return value;
	}
	
	protected void markAdded(StudentSubject studentSubject,StudentSubjectEvaluation studentSubjectEvaluation,BigDecimal value){}
	
	protected void sumMarks(StudentClassroomSessionDivisionReport r,Integer numberOfColumns){
		r.getMarkTotals().clear();
		//TODO must improved for performance issue
		for(int i=0;i<numberOfColumns;i++){
			r.getMarkTotals().add("0");
			for(StudentClassroomSessionDivisionSubjectReport cr : r.getSubjects()){
				r.getMarkTotals().set(i, format(new BigDecimal(StringUtils.replace(cr.getMarks().get(i), Constant.CHARACTER_COMA.toString(), Constant.CHARACTER_DOT.toString()))
					.add(new BigDecimal(StringUtils.replace(r.getMarkTotals().get(i), Constant.CHARACTER_COMA.toString(), Constant.CHARACTER_DOT.toString())))));
			}
		}
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
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.grade", r.getAverageScale());
		labelValue("school.report.studentclassroomsessiondivision.block.overallresult.rank", r.getRank());
		
		r.setGradingScaleLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.gradingscale"));
		IntervalCollection evaluationIntervalCollection = ((StudentClassroomSessionDivision)r.getSource()).getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentClassroomSessionDivisionAverageScale();
		rootBusinessLayer.getIntervalCollectionBusiness().load(evaluationIntervalCollection);
		for(Interval interval : evaluationIntervalCollection.getCollection()){
			LabelValueReport labelValueReport = new LabelValueReport(currentLabelValueCollection,null, interval.getCode(), interval.getName());
			labelValueReport.addExtendedValues(format(interval.getLow().getValue())+" - "+format(interval.getHigh().getValue()));
			currentLabelValueCollection.getCollection().add(labelValueReport);
		}
		
		r.setEffortLevelLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.effort"));
		IntervalCollection intervalCollection = ((StudentClassroomSessionDivision)r.getSource()).getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentWorkMetricCollection().getValueIntervalCollection();
		rootBusinessLayer.getIntervalCollectionBusiness().load(intervalCollection);
		for(Interval interval : intervalCollection.getCollection()){
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
			for(MetricValue metricValue : studentClassroomSessionDivision.getResults().getMetricValues())
				if(metricValue.getMetric().equals(metric)){
					value = format(metricValue.getValue());
					break;
				}
			LabelValueReport labelValueReport = new LabelValueReport(currentLabelValueCollection,null, metric.getName(), value);
			currentLabelValueCollection.getCollection().add(labelValueReport);
		}
	}

}
