package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.file.report.ReportManager;
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
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;

@Stateless
public class StudentClassroomSessionDivisionBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSessionDivision, StudentClassroomSessionDivision, StudentClassroomSessionDivisionDao, StudentSubject> implements StudentClassroomSessionDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private JasperReportBusinessImpl reportBusiness;
	@Inject private ReportManager reportManager;
	
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao; 
	
	@Inject 
	public StudentClassroomSessionDivisionBusinessImpl(StudentClassroomSessionDivisionDao dao) {
		super(dao); 
	}
	
	private void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		logTrace("Computing Student ClassroomSessionDivision Report of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent(),studentClassroomSessionDivision.getClassroomSessionDivision());		
		StudentClassroomSessionDivisionReport report = SchoolBusinessLayer.getInstance().getReportProducer().produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision);
		reportManager.buildBinaryContent(studentClassroomSessionDivision,report
				,studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations()
				.getStudentClassroomSessionDivisionResultsReportFile(),Boolean.TRUE);
	}
	
	@Override
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		return null;
	}
	
	@Override 
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean print) {
		logTrace("Computing Student ClassroomSessionDivision Report of {} ClassroomSessionDivision", classroomSessionDivisions.size());
		ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> report = new ReportBasedOnTemplateFile<>();
		/*
		 * Data loading
		 */
		Collection<StudentSubjectEvaluation> studentSubjectEvaluations = evaluatedStudentDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<StudentSubject> studentSubjects = studentSubjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = dao.readByClassroomSessionDivisions(classroomSessionDivisions);
		
		Collection<Lecture> lectures = lectureDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<Event> events = lectureDao.readEvents(lectures);
		Collection<EventParticipation> participations = eventParticipationDao.readByEvents(events);
		Collection<EventMissed> eventMisseds = eventMissedDao.readByEventParticipations(participations);
		
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		logTrace("Loaded data. StudentSubjectEvaluation={} , StudentSubject={} , StudentClassroomSessionDivision={} , Lecture={}"
				,studentSubjectEvaluations.size(),studentSubjects.size(),studentClassroomSessionDivisions.size(),lectures.size());
		/*
		 * Data computing
		 */
		studentSubjectBusiness.average(subjects, studentSubjects, studentSubjectEvaluations, Boolean.FALSE);
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
	
	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSessionDivision> levels) {
		return lectureDao.readByClassroomSessionDivisions(levels);
	}

	@Override
	protected ClassroomSessionDivision level(Lecture lecture) {
		return lecture.getSubject().getClassroomSessionDivision();
	}
	
}
