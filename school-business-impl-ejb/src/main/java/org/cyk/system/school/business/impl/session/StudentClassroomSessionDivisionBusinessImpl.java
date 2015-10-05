package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.file.report.jasper.JasperReportBusinessImpl;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

@Stateless
public class StudentClassroomSessionDivisionBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSessionDivision, StudentClassroomSessionDivision, StudentClassroomSessionDivisionDao, StudentSubject> implements StudentClassroomSessionDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private LanguageBusiness languageBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private JasperReportBusinessImpl reportBusiness;
	@Inject private FileBusiness fileBusiness;
	
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao; 
	
	@Inject 
	public StudentClassroomSessionDivisionBusinessImpl(StudentClassroomSessionDivisionDao dao) {
		super(dao); 
	}
	
	@Override 
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> resultsReportByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean print) {
		ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> report = new ReportBasedOnTemplateFile<>();
		/*
		 * Data loading
		 */
		Collection<StudentSubjectEvaluation> evaluatedStudents = evaluatedStudentDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<StudentSubject> studentSubjects = studentSubjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = dao.readByClassroomSessionDivisions(classroomSessionDivisions);
		
		Collection<Lecture> lectures = lectureDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<Event> events = lectureDao.readEvents(lectures);
		Collection<EventParticipation> participations = eventParticipationDao.readByEvents(events);
		Collection<EventMissed> eventMisseds = eventMissedDao.readByEventParticipations(participations);
		
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		
		/*
		 * Data computing
		 */
		studentSubjectBusiness.average(subjects, studentSubjects, evaluatedStudents, Boolean.FALSE);
		average(classroomSessionDivisions, studentClassroomSessionDivisions, studentSubjects, Boolean.FALSE);
		
		attendance(classroomSessionDivisions, studentClassroomSessionDivisions, lectures, participations, eventMisseds);
		
		//rank
		RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
        rankOptions.setType(RankType.EXAEQUO); 
        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
		studentSubjectBusiness.rank(subjects, studentSubjects,rankOptions);
		rank(classroomSessionDivisions, studentClassroomSessionDivisions,rankOptions);
		
		classroomSessionDivisionBusiness.results(classroomSessionDivisions, studentClassroomSessionDivisions);
		
		resultsReport(report, studentClassroomSessionDivisions, print);
		return report;
	}
	
	private void resultsReport(ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> report,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions,Boolean print) {
		/*
		for(StudentClassroomSessionDivision s : studentClassroomSessionDivisions){
			StudentClassroomSessionDivisionReport r = new StudentClassroomSessionDivisionReport();
			ClassroomSessionDivision csd = s.getClassroomSessionDivision();
			ClassroomSession cs = s.getClassroomSessionDivision().getClassroomSession();
			AcademicSession as = s.getClassroomSessionDivision().getClassroomSession().getAcademicSession();
			NodeResults results = csd.getResults();
			
			r.setAcademicSession(timeBusiness.formatPeriodFromTo(as.getPeriod()));
			r.setAppreciation(s.getResults().getAppreciation());
			r.setAppreciationCommentedBy(cs.getCoordinator().getPerson().getNames());
			r.setAverage(s.getResults().getEvaluationSort().getAverage().getValue().toString());
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
			report.getDataSource().add(r);
		}
		*/
		report.setTemplateFile(studentClassroomSessionDivisions.iterator().next().getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentClassroomSessionDivisionResultsReportFile());
		report.setFileExtension("pdf");
		resultsReport(report, print);
	}
		
	private void resultsReport(ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> aReport,Boolean print) {
		String fileName = /*languageBusiness.findText("school.student.results.report")*/"StudReport"+" "+System.currentTimeMillis();
		aReport.setFileName(fileName);
		//aReport.setTemplateFile(aReport.getDataSource().iterator().next().gets);
		reportBusiness.build(aReport, print);
	}
	
	@Override
	protected WeightedValue weightedValue(StudentSubject detail) {
		return new WeightedValue(detail.getResults().getEvaluationSort().getAverage().getValue(),detail.getClassroomSessionDivisionSubject().getCoefficient(),Boolean.FALSE);
	}

	@Override
	protected Student student(StudentSubject detail) {
		return detail.getStudent();
	}

	@Override
	protected Collection<StudentClassroomSessionDivision> readResults(Collection<ClassroomSessionDivision> levels) {
		return dao.readByClassroomSessionDivisions(levels);
	}

	@Override
	protected Collection<StudentSubject> readDetails(Collection<ClassroomSessionDivision> levels,Boolean keepDetails) {
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessionDivisions(levels);
		Collection<StudentSubject> studentSubjects = studentSubjectDao.readByClassroomSessionDivisions(levels);
		Collection<StudentSubjectEvaluation> evaluatedStudents = evaluatedStudentDao.readByClassroomSessionDivisions(levels);
		
		studentSubjectBusiness.average(subjects, studentSubjects, evaluatedStudents,keepDetails);
		
		return studentSubjects;
	}
	
	@Override
	protected ClassroomSessionDivision level(StudentClassroomSessionDivision result) {
		return result.getClassroomSessionDivision();
	}
	
	@Override
	protected ClassroomSessionDivision level(StudentSubject detail) {
		return detail.getClassroomSessionDivisionSubject().getClassroomSessionDivision();
	}
	
	@Override
	protected IntervalCollection averageIntervalCollection(ClassroomSessionDivision classroomSessionDivision) {
		return classroomSessionDivision.getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentClassroomSessionDivisionAverageScale();
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentClassroomSessionDivision findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
	} 

	/**/
	/*
    @AllArgsConstructor
	private class StudentSubjectWeight implements Weightable,Serializable {

		private static final long serialVersionUID = -7151566926896987903L;
		
		private StudentSubject studentSubject;
		
		@Override
		public BigDecimal getValue() {
			if(studentSubject.getResults().getAverage()==null)
				return null;
			return studentSubject.getResults().getAverage().multiply(studentSubject.getSubject().getCoefficient());
		}

		@Override
		public BigDecimal getWeight() {
			return studentSubject.getSubject().getCoefficient();
		}

	}
	*/

	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSessionDivision> levels) {
		return lectureDao.readByClassroomSessionDivisions(levels);
	}

	@Override
	protected ClassroomSessionDivision level(Lecture lecture) {
		return lecture.getSubject().getClassroomSessionDivision();
	}
	
}
