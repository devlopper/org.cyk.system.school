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

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.FormatterBusiness;
import org.cyk.system.root.business.api.file.report.ReportBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.root.persistence.api.mathematics.MetricDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.SchoolReportProducer.StudentClassroomSessionDivisionReportParameters;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.StudentResultsMetricValueDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionStudentsMetricCollectionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.cdi.BeanAdapter;

@Stateless
public class StudentClassroomSessionDivisionBusinessImpl extends AbstractStudentResultsBusinessImpl<StudentClassroomSessionDivision, StudentClassroomSessionDivisionDao,ClassroomSessionDivision, StudentClassroomSessionDivisionSubject> implements StudentClassroomSessionDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionSubjectBusiness studentSubjectBusiness;
	@Inject private StudentClassroomSessionDao studentClassroomSessionDao;
	private ReportBusiness reportBusiness = inject(ReportBusiness.class);
	
	@Inject private StudentClassroomSessionDivisionSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao; 
	@Inject private StudentResultsMetricValueDao studentResultsMetricValueDao;
	@Inject private MetricDao metricDao;
	@Inject private ClassroomSessionDivisionStudentsMetricCollectionDao classroomSessionDivisionStudentsMetricCollectionDao;
	
	
	@Inject 
	public StudentClassroomSessionDivisionBusinessImpl(StudentClassroomSessionDivisionDao dao) {
		super(dao); 
	}
	
	@Override
	protected Object[] getPropertyValueTokens(StudentClassroomSessionDivision studentClassroomSessionDivision, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{studentClassroomSessionDivision.getStudent(),studentClassroomSessionDivision.getClassroomSessionDivision()};
		return super.getPropertyValueTokens(studentClassroomSessionDivision, name);
	}
	
	@Override
	public StudentClassroomSessionDivision create(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super.create(studentClassroomSessionDivision);
		
		Student student = studentClassroomSessionDivision.getStudent();
		ClassroomSessionDivision classroomSessionDivision = studentClassroomSessionDivision.getClassroomSessionDivision();
		ClassroomSession classroomSession = classroomSessionDivision.getClassroomSession();
		
		StudentClassroomSession studentClassroomSession = studentClassroomSessionDao.readByStudentByClassroomSession(student, classroomSession);
		if(studentClassroomSession==null){
			studentClassroomSession = new StudentClassroomSession(student, classroomSession);
			studentClassroomSession.setCascadeOperationToChildren(studentClassroomSessionDivision.getCascadeOperationToChildren());
			studentClassroomSession.setCascadeOperationToMaster(studentClassroomSessionDivision.getCascadeOperationToMaster());
			inject(StudentClassroomSessionBusiness.class).create(studentClassroomSession);
		}
		
		Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = classroomSessionDivisionStudentsMetricCollectionDao.readByClassroomSessionDivision(classroomSessionDivision);
		
		for(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection : classroomSessionDivisionStudentsMetricCollections)
			for(Metric metric : metricDao.readByCollection(classroomSessionDivisionStudentsMetricCollection.getMetricCollection())){
				studentClassroomSessionDivision.getResults().getStudentResultsMetricValues()
					.add(new StudentResultsMetricValue(studentClassroomSessionDivision.getResults(), new MetricValue(metric, null,null,null)));
			}
		
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = new ArrayList<>();
		if(Boolean.TRUE.equals(studentClassroomSessionDivision.getCascadeOperationToChildren())){
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : subjectDao.readByClassroomSessionDivision(classroomSessionDivision)){
				studentSubjects.add(new StudentClassroomSessionDivisionSubject(student, classroomSessionDivisionSubject));
			}
		}
		cascade(studentClassroomSessionDivision, studentClassroomSessionDivision.getResults().getStudentResultsMetricValues(), studentSubjects, Crud.CREATE);
		
		return studentClassroomSessionDivision;
	}
	
	private void cascade(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues
			,Collection<StudentClassroomSessionDivisionSubject> studentSubjects,Crud crud){

		new CascadeOperationListener.Adapter.Default<StudentResultsMetricValue,StudentResultsMetricValueDao,StudentResultsMetricValueBusiness>(studentResultsMetricValueDao,null)
			.operate(studentResultsMetricValues, crud);
	
		logTrace("Student classroomsession division. {} , {} : {}", studentClassroomSessionDivision.getStudent().getCode(),studentClassroomSessionDivision.getClassroomSessionDivision().getIdentifier(),crud);
		
		new CascadeOperationListener.Adapter.Default<StudentClassroomSessionDivisionSubject,StudentClassroomSessionDivisionSubjectDao,StudentClassroomSessionDivisionSubjectBusiness>(null,inject(StudentClassroomSessionDivisionSubjectBusiness.class))
			.operate(studentSubjects, crud);
	}
	
	@Override
	public StudentClassroomSessionDivision delete(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		cascade(studentClassroomSessionDivision, studentResultsMetricValueDao.readByStudentResults(studentClassroomSessionDivision.getResults())
				, studentSubjectDao.readByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent(),studentClassroomSessionDivision.getClassroomSessionDivision()), Crud.DELETE);
		return super.delete(studentClassroomSessionDivision);
	}
	
	@Override 
	public void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision,ServiceCallArguments arguments) {
		//logTrace("Building Student ClassroomSessionDivision Report of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent()
		//		,inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getClassroomSessionDivision()));
		if( (Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired()) 
				&& studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()!=null) || !Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired()) ){
			StudentClassroomSessionDivisionReportParameters parameters = 
					new StudentClassroomSessionDivisionReportParameters(SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS);
			
			CreateReportFileArguments<StudentClassroomSessionDivision> reportArguments = 
	    			new CreateReportFileArguments<StudentClassroomSessionDivision>(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET, studentClassroomSessionDivision);
			File file = createReportFile(studentClassroomSessionDivision, reportArguments);
			
			if(file==null){
				
			}else{
				genericDao.update(studentClassroomSessionDivision.getResults());
				logIdentifiable("Report built",studentClassroomSessionDivision);
			}
			
		}else{
			logTrace("Cannot build Student ClassroomSessionDivision Report of Student {} in ClassroomSessionDivision {}", studentClassroomSessionDivision.getStudent()
					,inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getClassroomSessionDivision()));
		}
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReportTemplateFile> findReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		return reportBusiness.buildBinaryContent(studentClassroomSessionDivision.getResults().getReport(), 
				studentClassroomSessionDivision.getStudent().getCode());
	}
	
	@Override
	public void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		buildReport(studentClassroomSessionDivision, new ServiceCallArguments());
	}
	
	@Override
	public ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReportTemplateFile> findReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {
		// TODO Many report as one document must be handled
		return findReport(studentClassroomSessionDivisions.iterator().next());
	}
		
	@Override 
	public void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean updateEvaluationResults,Boolean updateAttendanceResults,Boolean updateRankResults,RankOptions<SortableStudentResults> rankOptions,ServiceCallArguments callArguments) {
		if(Boolean.TRUE.equals(updateEvaluationResults))
			updateAverage(classroomSessionDivisions, callArguments);
		clearCallArgumentsExecution(callArguments);
		if(Boolean.TRUE.equals(updateAttendanceResults))
			updateAttendance(classroomSessionDivisions, callArguments);
		clearCallArgumentsExecution(callArguments);
		if(Boolean.TRUE.equals(updateRankResults))
			updateRank(classroomSessionDivisions,rankOptions, callArguments);
		clearCallArgumentsExecution(callArguments);
		
		logTrace("Computing Student ClassroomSessionDivision Report of {} ClassroomSessionDivision(s)", classroomSessionDivisions.size());
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = dao.readByClassroomSessionDivisions(classroomSessionDivisions);
		//debug(studentClassroomSessionDivisions.iterator().next().getClassroomSessionDivision().getResults());
		setCallArgumentsObjects(callArguments, studentClassroomSessionDivisions);
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
			if(callArguments!=null && callArguments.getExecutionProgress()!=null){
				callArguments.getExecutionProgress().setCurrentExecutionStep(inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession())
						+" - "+inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getStudent()));
			}
			if(Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired()) 
					&& studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()==null){
				logIdentifiable("Cannot build report", studentClassroomSessionDivision);
			}else{
				buildReport(studentClassroomSessionDivision);
			}
			
			addCallArgumentsWorkDoneByStep(callArguments);
		}
	}
	
	@Override
	public Collection<StudentClassroomSessionDivision> updateAverage(Collection<ClassroomSessionDivision> classroomSessionDivisions, BusinessServiceCallArguments<StudentClassroomSessionDivision> callArguments) {
		/*
		 * Data loading
		 */
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations = evaluatedStudentDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = dao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		logTrace("Loaded data. StudentSubjectEvaluation={} , StudentSubject={} , StudentClassroomSessionDivision={}"
				,studentSubjectEvaluations.size(),studentSubjects.size(),studentClassroomSessionDivisions.size());
		
		setCallArgumentsObjects(callArguments, studentClassroomSessionDivisions);
		/*
		 * Data computing
		 */
		
		studentSubjectBusiness.updateAverage(subjects, studentSubjects, studentSubjectEvaluations, null);
		updateAverage(classroomSessionDivisions, studentClassroomSessionDivisions, studentSubjects, callArguments);
		
		inject(ClassroomSessionDivisionSubjectBusiness.class).computeResults(subjects, studentSubjects);
		inject(ClassroomSessionDivisionBusiness.class).computeResults(classroomSessionDivisions, studentClassroomSessionDivisions);
		
		for(Listener listener : Listener.COLLECTION)
			listener.processOnEvaluationAverageUpdated(classroomSessionDivisions, callArguments);
		
		return studentClassroomSessionDivisions;
	}
	
	@Override
	public Collection<StudentClassroomSessionDivision> updateRank(Collection<ClassroomSessionDivision> classroomSessionDivisions,RankOptions<SortableStudentResults> options,BusinessServiceCallArguments<StudentClassroomSessionDivision> callArguments) {
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessionDivisions(classroomSessionDivisions);
		studentSubjectBusiness.updateRank(subjects, studentSubjects,options,null);
		return super.updateRank(classroomSessionDivisions, options,callArguments);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public void setNumberOfTimesAbsent(StudentClassroomSessionDivision studentClassroomSessionDivision,BigDecimal value) {
		studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDuration(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToMillisecond(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession(),value));

		studentClassroomSessionDivision.getResults().getLectureAttendance().setAttendedDuration(studentClassroomSessionDivision.getClassroomSessionDivision()
				.getExistencePeriod().getNumberOfMillisecond().getSystemAs(Long.class)-
				studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration());
	}
	
	/**/
	
	@Override
	protected Class<StudentClassroomSessionDivisionSubject> getDetailsClass() {
		return StudentClassroomSessionDivisionSubject.class;
	}
	
	@Override
	protected Class<StudentClassroomSessionDivision> getResultClass() {
		return StudentClassroomSessionDivision.class;
	}
				
	@Override
	protected WeightedValue weightedValue(StudentClassroomSessionDivisionSubject detail) {
		return new WeightedValue(detail.getResults().getEvaluationSort().getAverage().getValue(),detail.getClassroomSessionDivisionSubject().getWeight(),Boolean.FALSE);
	}

	@Override
	protected Student student(StudentClassroomSessionDivisionSubject detail) {
		return detail.getStudent();
	}

	@Override
	protected Collection<StudentClassroomSessionDivision> readResults(Collection<ClassroomSessionDivision> levels) {
		return dao.readByClassroomSessionDivisions(levels);
	}

	@Override
	protected Collection<StudentClassroomSessionDivisionSubject> readDetails(Collection<ClassroomSessionDivision> levels,Boolean keepDetails) {
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessionDivisions(levels);
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectDao.readByClassroomSessionDivisions(levels);
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> evaluatedStudents = evaluatedStudentDao.readByClassroomSessionDivisions(levels);
		
		studentSubjectBusiness.updateAverage(subjects, studentSubjects, evaluatedStudents,null);
		
		return studentSubjects;
	}
	
	@Override
	protected ClassroomSessionDivision level(StudentClassroomSessionDivision result) {
		return result.getClassroomSessionDivision();
	}
	
	@Override
	protected ClassroomSessionDivision level(StudentClassroomSessionDivisionSubject detail) {
		return detail.getClassroomSessionDivisionSubject().getClassroomSessionDivision();
	}
	
	@Override
	protected Boolean isLectureAttendanceAggregatable(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		return studentClassroomSessionDivision.getClassroomSessionDivision().getStudentSubjectAttendanceAggregated();
	}
	
	@Override
	protected Long getAttendableDuration(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		return studentClassroomSessionDivision.getClassroomSessionDivision().getExistencePeriod().getNumberOfMillisecond().getSystemAs(Long.class);
	}
	
	@Override
	protected IntervalCollection averageAppreciatedIntervalCollection(ClassroomSessionDivision classroomSessionDivision) {
		return classroomSessionDivision.getClassroomSession().getLevelTimeDivision().getLevel().getLevelName().getNodeInformations().getStudentClassroomSessionDivisionAverageScale();
	}
	
	@Override
	protected IntervalCollection averagePromotedIntervalCollection(ClassroomSessionDivision classroomSessionDivision) {
		return classroomSessionDivision.getClassroomSession().getLevelTimeDivision().getLevel().getLevelName().getNodeInformations().getStudentClassroomSessionAveragePromotionScale();
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
	public Collection<StudentClassroomSessionDivision> findByClassroomSessionDivisionOrderNumber(Long classroomSessionDivisionOrderNumber) {
		return dao.readByClassroomSessionDivisionIndex(classroomSessionDivisionOrderNumber);
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
	
	public static interface Listener {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		void processOnEvaluationAverageUpdated(Collection<ClassroomSessionDivision> classroomSessionDivisions, BusinessServiceCallArguments<StudentClassroomSessionDivision> callArguments);
		
		/**/
		
		public static class Adapter extends BeanAdapter implements Listener,Serializable {
			private static final long serialVersionUID = 2280338625270476061L;
			@Override
			public void processOnEvaluationAverageUpdated(Collection<ClassroomSessionDivision> classroomSessionDivisions,BusinessServiceCallArguments<StudentClassroomSessionDivision> callArguments) {}
			/**/
			
			public static class Default extends Adapter implements Serializable {
				private static final long serialVersionUID = 2280338625270476061L;
				
				/**/
				
				
			}
		}
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher) {
		return dao.readByClassroomSessionByTeacher(classroomSession,teacher);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivision> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return dao.readByLevelTimeDivision(levelTimeDivision);
	}
}
