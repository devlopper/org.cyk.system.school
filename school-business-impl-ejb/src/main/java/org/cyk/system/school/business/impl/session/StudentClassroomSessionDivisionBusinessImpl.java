package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.file.report.ReportBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SortableStudentResultsComparator;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.persistence.api.StudentResultsMetricValueDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;

@Stateless
public class StudentClassroomSessionDivisionBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSessionDivision, StudentClassroomSessionDivision, StudentClassroomSessionDivisionDao, StudentSubject> implements StudentClassroomSessionDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionDao studentClassroomSessionDao;
	private ReportBusiness reportBusiness = RootBusinessLayer.getInstance().getReportBusiness();
	
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao; 
	@Inject private StudentResultsMetricValueDao studentResultsMetricValueDao;
	
	
	@Inject 
	public StudentClassroomSessionDivisionBusinessImpl(StudentClassroomSessionDivisionDao dao) {
		super(dao); 
	}
	
	@Override
	public StudentClassroomSessionDivision create(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super.create(studentClassroomSessionDivision);
		
		if(studentClassroomSessionDivision.getResults()==null)
			studentClassroomSessionDivision.setResults(new StudentResults());
		
		Student student = studentClassroomSessionDivision.getStudent();
		ClassroomSessionDivision classroomSessionDivision = studentClassroomSessionDivision.getClassroomSessionDivision();
		ClassroomSession classroomSession = classroomSessionDivision.getClassroomSession();
		
		StudentClassroomSession studentClassroomSession = studentClassroomSessionDao.readByStudentByClassroomSession(student, classroomSession);
		if(studentClassroomSession==null){
			schoolBusinessLayer.getStudentClassroomSessionBusiness().create(new StudentClassroomSession(student, classroomSession));
		}
		
		MetricCollection metricCollection = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNodeInformations()
				.getStudentWorkMetricCollection();
		RootBusinessLayer.getInstance().getMetricCollectionBusiness().load(metricCollection);
		IntervalCollection intervalCollection = metricCollection.getValueIntervalCollection();
		RootBusinessLayer.getInstance().getIntervalCollectionBusiness().load(intervalCollection);
		for(Metric metric : metricCollection.getCollection()){
			studentClassroomSessionDivision.getResults().getStudentResultsMetricValues()
				.add(new StudentResultsMetricValue(studentClassroomSessionDivision.getResults(), new MetricValue(metric, BigDecimal.ZERO)));
		}
		for(StudentResultsMetricValue studentResultsMetricValue : studentClassroomSessionDivision.getResults().getStudentResultsMetricValues()){
			genericDao.create(studentResultsMetricValue.getMetricValue());
			studentResultsMetricValueDao.create(studentResultsMetricValue);
		}
		
		logTrace("Student {} for classroomsession division {} registered", student,classroomSessionDivision);
		return studentClassroomSessionDivision;
	}
	
	@Override
	public void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision,BuildReportOptions options) {
		logTrace("Computing Student ClassroomSessionDivision Report of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent(),studentClassroomSessionDivision.getClassroomSessionDivision());
		StudentClassroomSessionDivisionReport report = SchoolBusinessLayer.getInstance().getReportProducer().produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision
				,SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS);
		
		reportBusiness.buildBinaryContent(studentClassroomSessionDivision.getResults(),report
				,studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations()
				.getStudentClassroomSessionDivisionResultsReportFile(),Boolean.TRUE);
		dao.update(studentClassroomSessionDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		return reportBusiness.buildBinaryContent(studentClassroomSessionDivision.getResults().getReport(), 
				studentClassroomSessionDivision.getStudent().getRegistration().getCode());
	}
	
	@Override
	public void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		buildReport(studentClassroomSessionDivision, StudentClassroomSessionDivisionBusiness.DEFAULT_BUILD_REPORT_OPTIONS);
	}
	
	@Override
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {
		// TODO Many report as one document must be handled
		return findReport(studentClassroomSessionDivisions.iterator().next());
	}
	
	@Override
	public void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		buildReport(classroomSessionDivisions, StudentClassroomSessionDivisionBusiness.DEFAULT_BUILD_REPORT_OPTIONS);
	}
	
	@Override 
	public void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,BuildReportOptions options) {
		logTrace("Computing Student ClassroomSessionDivision Report of {} ClassroomSessionDivision(s)", classroomSessionDivisions.size());
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
		
		if(Boolean.TRUE.equals(options.getAttendance())){
			attendance(classroomSessionDivisions, studentClassroomSessionDivisions, lectures, participations, eventMisseds);
		}
		//rank
		RankOptions<SortableStudentResults> rankOptions = new RankOptions<>();
        rankOptions.setType(RankType.EXAEQUO); 
        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
		studentSubjectBusiness.rank(subjects, studentSubjects,rankOptions);
		rank(classroomSessionDivisions, studentClassroomSessionDivisions,rankOptions);
		
		SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().computeResults(subjects, studentSubjects);
		
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions)
			buildReport(studentClassroomSessionDivision);
		
		classroomSessionDivisionBusiness.results(classroomSessionDivisions, studentClassroomSessionDivisions);
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
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<StudentClassroomSessionDivision> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
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

	/*
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public StudentClassroomSessionDivision prepareUpdateOfMetricValues(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		studentClassroomSessionDivision.getResults()
			.setStudentResultsMetricValues(studentResultsMetricValueDao.readByStudentResults(studentClassroomSessionDivision.getResults()));
		if(studentClassroomSessionDivision.getResults().getStudentResultsMetricValues().isEmpty()){
			MetricCollection metricCollection = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNodeInformations()
					.getStudentWorkMetricCollection();
			RootBusinessLayer.getInstance().getMetricCollectionBusiness().load(metricCollection);
			IntervalCollection intervalCollection = metricCollection.getValueIntervalCollection();
			RootBusinessLayer.getInstance().getIntervalCollectionBusiness().load(intervalCollection);
			for(Metric metric : metricCollection.getCollection())
				studentClassroomSessionDivision.getResults().getStudentResultsMetricValues()
					.add(new StudentResultsMetricValue(studentClassroomSessionDivision.getResults()
							, new MetricValue(metric, new BigDecimal(RandomDataProvider.getInstance().randomInt(intervalCollection.getLowestValue().intValue(), intervalCollection.getHighestValue().intValue())))));
		}
		return studentClassroomSessionDivision;
	}*/
	

	@Override
	public StudentClassroomSessionDivision update(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues) {
		update(studentClassroomSessionDivision);
		studentClassroomSessionDivision.getResults().setStudentResultsMetricValues(studentResultsMetricValues);
		delete(StudentResultsMetricValue.class,studentResultsMetricValueDao,studentResultsMetricValueDao.readByStudentResults(studentClassroomSessionDivision.getResults()),
				studentClassroomSessionDivision.getResults().getStudentResultsMetricValues());
		for(StudentResultsMetricValue studentResultsMetricValue : studentClassroomSessionDivision.getResults().getStudentResultsMetricValues())
			if(studentResultsMetricValue.getIdentifier()==null){
				if(studentResultsMetricValue.getMetricValue().getIdentifier()==null)
					genericDao.create(studentResultsMetricValue.getMetricValue());
				studentResultsMetricValueDao.create(studentResultsMetricValue);
			}else
				studentResultsMetricValueDao.update(studentResultsMetricValue);
		return studentClassroomSessionDivision;
	}
	
}
