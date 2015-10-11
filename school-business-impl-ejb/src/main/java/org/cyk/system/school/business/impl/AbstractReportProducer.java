package org.cyk.system.school.business.impl;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractRootBusinessBean;
import org.cyk.system.school.business.api.session.ReportProducer;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;

public abstract class AbstractReportProducer extends AbstractRootBusinessBean implements ReportProducer,Serializable {

	private static final long serialVersionUID = 4631829200070130087L;

	@Override
	public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		StudentClassroomSessionDivisionReport r = new StudentClassroomSessionDivisionReport();
		
		StudentClassroomSessionDivision s = studentClassroomSessionDivision;
		ClassroomSessionDivision csd = s.getClassroomSessionDivision();
		ClassroomSession cs = s.getClassroomSessionDivision().getClassroomSession();
		AcademicSession as = s.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
		NodeResults results = csd.getResults();
		
		r.getAcademicSession().setFromDateToDate(timeBusiness.formatPeriodFromTo(as.getPeriod()));
		r.setComments(s.getResults().getAppreciation());
		//r.getCommentator().getPerson().setNames(cs.getCoordinator().getPerson().getNames());
		//r.setAverage(s.getResults().getEvaluationSort().getAverage().getValue().toString());
		
		/*
		r.setClassroomSession(cs.getUiString());
		r.setClassroomSessionAverage(results.getAverage().toString());
		r.setClassroomSessionAverageHighest(results.getAverageHighest().toString());
		r.setClassroomSessionAverageLowest(results.getAverageLowest().toString());
		r.setDateOfBirth(timeBusiness.formatDate(s.getStudent().getPerson().getBirthDate()));
		r.setFooter("Infos sur contacts ici");
		
		r.setNames(s.getStudent().getPerson().getNames());
		r.setNumberOfStudents(numberBusiness.format(results.getNumberOfStudent()));
		r.setOrderNumber(s.getStudent().getRegistration().getCode());
		
		if(s.getStudent().getPerson().getImage()==null)
			;
		else
			r.setPhoto(fileBusiness.findInputStream(s.getStudent().getPerson().getImage()));
		
		r.setRank(mathematicsBusiness.format(s.getResults().getEvaluationSort().getRank()));
		r.setSchoolLogo(fileBusiness.findInputStream(as.getSchool().getOwnedCompany().getCompany().getImage()));
		
		r.setSchoolName(as.getSchool().getOwnedCompany().getCompany().getName());
		r.setSignatureInfos(timeBusiness.formatDate(new Date()));
		r.setStaffPerson("PersonWhoSign");
		r.setStaffTitle(languageBusiness.findText("school.report.student.division.results.staff.title"));
		r.setTitle(languageBusiness.findText("school.report.student.division.results.title",new Object[]{csd.getUiString()}));
		
		r.setTotalCoefficient(s.getResults().getEvaluationSort().getAverage().getDivisor().toString());
		r.setTotalAverageCoefficiented(s.getResults().getEvaluationSort().getAverage().getDividend().toString());
		
		r.setTotalMissedHours((s.getResults().getLectureAttendance().getMissedDuration()/DateUtils.MILLIS_PER_HOUR) +"");
		r.setTotalMissedHoursJustified((s.getResults().getLectureAttendance().getMissedDurationJustified()/DateUtils.MILLIS_PER_HOUR)+"");
		
		for(StudentSubject studentSubject : s.getDetails()){
			StudentClassroomSessionDivisionSubjectReport sr = new StudentClassroomSessionDivisionSubjectReport();
			sr.setReport(r);
			sr.setAppreciation(studentSubject.getResults().getAppreciation());
			sr.setAverage(studentSubject.getResults().getEvaluationSort().getAverage().getValue().toString());
			sr.setCoefficient(studentSubject.getClassroomSessionDivisionSubject().getCoefficient().toString());
			sr.setAverageCoefficiented(studentSubject.getResults().getEvaluationSort().getAverage().getValue().multiply(studentSubject.getClassroomSessionDivisionSubject().getCoefficient()).toString());
			sr.setName(studentSubject.getClassroomSessionDivisionSubject().getSubject().getName());
			sr.setRank(mathematicsBusiness.format(studentSubject.getResults().getEvaluationSort().getRank()));
			sr.setTeacherNames(studentSubject.getClassroomSessionDivisionSubject().getTeacher().getPerson().getNames());
			r.getSubjects().add(sr);
		}
		*/
		
		return r;
	}

}
