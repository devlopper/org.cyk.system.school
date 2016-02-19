package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.file.report.ReportBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.root.persistence.api.mathematics.MetricDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.SchoolReportProducer.StudentClassroomSessionDivisionReportParameters;
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
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.cdi.BeanAdapter;

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
	@Inject private MetricDao metricDao;
	
	
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
			studentClassroomSession = new StudentClassroomSession(student, classroomSession);
			studentClassroomSession.setCascadeTopDownOnCreate(studentClassroomSessionDivision.getCascadeTopDownOnCreate());
			studentClassroomSession.setCascadeBottomUpOnCreate(studentClassroomSessionDivision.getCascadeBottomUpOnCreate());
			schoolBusinessLayer.getStudentClassroomSessionBusiness().create(studentClassroomSession);
		}
		
		MetricCollection metricCollection = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
				.findCommonNodeInformations(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()).getStudentWorkMetricCollection();
		for(Metric metric : metricDao.readByCollection(metricCollection)){
			studentClassroomSessionDivision.getResults().getStudentResultsMetricValues()
				.add(new StudentResultsMetricValue(studentClassroomSessionDivision.getResults(), new MetricValue(metric, BigDecimal.ZERO,Constant.EMPTY_STRING.toString(),null)));
		}
		
		Collection<StudentSubject> studentSubjects = new ArrayList<>();
		if(Boolean.TRUE.equals(studentClassroomSessionDivision.getCascadeTopDownOnCreate())){
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : subjectDao.readByClassroomSessionDivision(classroomSessionDivision)){
				studentSubjects.add(new StudentSubject(student, classroomSessionDivisionSubject));
			}
		}
		cascade(studentClassroomSessionDivision, studentClassroomSessionDivision.getResults().getStudentResultsMetricValues(), studentSubjects, Crud.CREATE);
		
		return studentClassroomSessionDivision;
	}
	
	private void cascade(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues
			,Collection<StudentSubject> studentSubjects,Crud crud){

		new CascadeOperationListener.Adapter.Default<StudentResultsMetricValue,StudentResultsMetricValueDao,StudentResultsMetricValueBusiness>(studentResultsMetricValueDao,null)
			.operate(studentResultsMetricValues, crud);
	
		logTrace("Student classroomsession division. {} , {} : {}", studentClassroomSessionDivision.getStudent().getRegistration().getCode(),studentClassroomSessionDivision.getClassroomSessionDivision().getIdentifier(),crud);
		
		new CascadeOperationListener.Adapter.Default<StudentSubject,StudentSubjectDao,StudentSubjectBusiness>(null,SchoolBusinessLayer.getInstance().getStudentSubjectBusiness())
			.operate(studentSubjects, crud);
	}
	
	@Override
	public StudentClassroomSessionDivision delete(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		cascade(studentClassroomSessionDivision, studentResultsMetricValueDao.readByStudentResults(studentClassroomSessionDivision.getResults())
				, studentSubjectDao.readByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent(),studentClassroomSessionDivision.getClassroomSessionDivision()), Crud.DELETE);
		return super.delete(studentClassroomSessionDivision);
	}
	
	@Override
	public void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision,BuildReportArguments arguments) {
		//logTrace("Building Student ClassroomSessionDivision Report of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent()
		//		,RootBusinessLayer.getInstance().getFormatterBusiness().format(studentClassroomSessionDivision.getClassroomSessionDivision()));
		if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()==null){
			logTrace("Cannot build Student ClassroomSessionDivision Report of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent()
					,RootBusinessLayer.getInstance().getFormatterBusiness().format(studentClassroomSessionDivision.getClassroomSessionDivision()));
		}else{
			StudentClassroomSessionDivisionReportParameters parameters = 
					new StudentClassroomSessionDivisionReportParameters(SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS);
			
			if(arguments.getRankable()==null){
				
			}else
				parameters.setRankable(arguments.getRankable());
			
			StudentClassroomSessionDivisionReport report = SchoolBusinessLayer.getInstance().getReportProducer().produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision
					,parameters);
			
			if(report==null){
				
			}else{
				if(studentClassroomSessionDivision.getResults().getReport()==null)
					studentClassroomSessionDivision.getResults().setReport(new File());
				reportBusiness.buildBinaryContent(studentClassroomSessionDivision.getResults(),report
					,studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations()
					.getStudentClassroomSessionDivisionResultsReportTemplate().getTemplate(),Boolean.TRUE);				
				//dao.update(studentClassroomSessionDivision);
				genericDao.update(studentClassroomSessionDivision.getResults());
				logIdentifiable("Report built",studentClassroomSessionDivision);
			}
		}
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
	public void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,BuildReportArguments options) {
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
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
			if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()==null){
				logIdentifiable("Cannot build report", studentClassroomSessionDivision);
				//debug(studentClassroomSessionDivision.getResults());
				//debug(studentClassroomSessionDivision.getResults().getEvaluationSort());
				//debug(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage());
				//logTrace("Building of Student ClassroomSessionDivision Report will be skipped for of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent()
				//		,RootBusinessLayer.getInstance().getFormatterBusiness().format(studentClassroomSessionDivision.getClassroomSessionDivision()));
			}else{
				buildReport(studentClassroomSessionDivision);
			}
		}
		
		classroomSessionDivisionBusiness.computeResults(classroomSessionDivisions, studentClassroomSessionDivisions);
	}
	
	@Override
	protected Class<StudentSubject> getDetailsClass() {
		return StudentSubject.class;
	}
	
	@Override
	protected Class<StudentClassroomSessionDivision> getResultClass() {
		return StudentClassroomSessionDivision.class;
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
		return findByClassroomSessionDivisions(Arrays.asList(classroomSessionDivision));
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return dao.readByClassroomSessionDivisions(classroomSessionDivisions);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentClassroomSessionDivision findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
	} 
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<File> findReportFiles(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {
		Collection<File> files = new ArrayList<>();
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions)
			if(studentClassroomSessionDivision.getResults().getReport()!=null)
				files.add(studentClassroomSessionDivision.getResults().getReport());
		return files;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisionIndex(Byte classroomSessionDivisionIndex) {
		return dao.readByClassroomSessionDivisionIndex(classroomSessionDivisionIndex);
	}

	/**/
	
	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSessionDivision> levels) {
		return lectureDao.readByClassroomSessionDivisions(levels);
	}

	@Override
	protected ClassroomSessionDivision level(Lecture lecture) {
		return lecture.getClassroomSessionDivisionSubject().getClassroomSessionDivision();
	}

	@Override
	public StudentClassroomSessionDivision update(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues) {
		update(studentClassroomSessionDivision);
		studentClassroomSessionDivision.getResults().setStudentResultsMetricValues(studentResultsMetricValues);
		delete(StudentResultsMetricValue.class,studentResultsMetricValueDao,studentResultsMetricValueDao.readByStudentResults(studentClassroomSessionDivision.getResults()),
			studentClassroomSessionDivision.getResults().getStudentResultsMetricValues());
		for(StudentResultsMetricValue studentResultsMetricValue : studentClassroomSessionDivision.getResults().getStudentResultsMetricValues()){
			studentResultsMetricValueDao.update(studentResultsMetricValue);
		}
			
		return studentClassroomSessionDivision;
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByStudentByClassroomSession(Student student,ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student,classroomSession);
	}
	
	/**/
	
	public static interface Listener{
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		
		
		/**/
		
		public static class Adapter extends BeanAdapter implements Listener,Serializable{
			private static final long serialVersionUID = -9048282379616583423L;
			
			
			/**/
			
			public static class Default extends Adapter implements Serializable{
				private static final long serialVersionUID = 2884910167320359611L;
				
			}
		}
	}
	
}
