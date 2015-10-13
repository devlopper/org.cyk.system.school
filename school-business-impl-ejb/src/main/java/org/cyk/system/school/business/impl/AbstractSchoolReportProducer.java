package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;

import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.root.business.impl.RootBusinessLayer;
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

public abstract class AbstractSchoolReportProducer extends AbstractCompanyReportProducer implements SchoolReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	@Override
	public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		StudentClassroomSessionDivisionReport r = new StudentClassroomSessionDivisionReport();
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
		r.getClassroomSessionDivision().setAverage(results.getAverage().toString());
		r.getClassroomSessionDivision().setHighestAverage(results.getAverageHighest().toString());
		r.getClassroomSessionDivision().setLowestAverage(results.getAverageLowest().toString());
		r.getClassroomSessionDivision().setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		
		set(student, r.getStudent());
		set(cs.getCoordinator(), r.getCommentator());
		set(as.getSchool().getOwnedCompany().getCompany().getManager(), r.getSigner());
		
		r.setComments(s.getResults().getAppreciation());
		r.setAverage(format(s.getResults().getEvaluationSort().getAverage().getValue()));
		r.setAverageScale(s.getResults().getEvaluationSort().getAverageInterval().getName());
		r.setRank(RootBusinessLayer.getInstance().getMathematicsBusiness().format(s.getResults().getEvaluationSort().getRank()));
		r.setName(languageBusiness.findText("school.report.studentclassroomsessiondivision.title",new Object[]{csd.getUiString()}));
		r.setSubjectsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.subject"));
		r.setCommentsBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.comments"));
		r.setSchoolStampBlockTitle(languageBusiness.findText("school.report.studentclassroomsessiondivision.block.schoolstamp"));
		
		r.setTotalCoefficient(s.getResults().getEvaluationSort().getAverage().getDivisor().toString());
		r.setTotalAverageCoefficiented(s.getResults().getEvaluationSort().getAverage().getDividend().toString());
		
		r.setMissedTime((s.getResults().getLectureAttendance().getMissedDuration()/DateUtils.MILLIS_PER_HOUR) +"");
		r.setMissedTimeJustified((s.getResults().getLectureAttendance().getMissedDurationJustified()/DateUtils.MILLIS_PER_HOUR)+"");
		
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
			for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubject.getDetails()){
				BigDecimal value = studentSubjectEvaluation.getValue();
				if(Boolean.FALSE.equals(studentSubjectEvaluation.getSubjectEvaluation().getCoefficientApplied()))
					value = value.multiply(studentSubjectEvaluation.getSubjectEvaluation().getType().getCoefficient());
				sr.getMarks().add(format(value));
			}
			r.getSubjects().add(sr);
		}
				
		produceStudentClassroomSessionDivisionReportLabelValueCollections(r);
			
		return r;
	}
	
	protected void produceStudentClassroomSessionDivisionReportLabelValueCollections(StudentClassroomSessionDivisionReport r){
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
	}

}
