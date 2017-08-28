package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession.SearchCriteria;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionSubjectDao;

@Stateless
public class StudentClassroomSessionBusinessImpl extends AbstractStudentResultsBusinessImpl<StudentClassroomSession,StudentClassroomSessionDao,ClassroomSession, StudentClassroomSessionDivision> implements StudentClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionDivisionSubjectBusiness studentSubjectBusiness;
	
	@Inject private StudentClassroomSessionDivisionSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao;
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public StudentClassroomSessionBusinessImpl(StudentClassroomSessionDao dao) {
		super(dao); 
	}
	
	@Override
	public StudentClassroomSession instanciateOne(Student student, ClassroomSession classroomSession) {
		StudentClassroomSession studentClassroomSession = instanciateOne();
		studentClassroomSession.setStudent(student);
		studentClassroomSession.setClassroomSession(classroomSession);
		if(Boolean.TRUE.equals(classroomSession.getLevelTimeDivision().getLevel().getLevelName().getAllClassroomSessionDivisionSubjectRequired())){
			for(ClassroomSessionSubject classroomSessionSubject : inject(ClassroomSessionSubjectDao.class).readByClassroomSession(classroomSession))
				studentClassroomSession.getStudentClassroomSessionSubjects().addOne(new StudentClassroomSessionSubject(student, classroomSessionSubject));
		}
		return studentClassroomSession;
	}
	
	@Override
	public StudentClassroomSession instanciateOne(ClassroomSession classroomSession,Student student) {
		return instanciateOne(student, classroomSession);
	}
	
	@Override
	protected void afterCreate(StudentClassroomSession studentClassroomSession) {
		super.afterCreate(studentClassroomSession);
		if(Boolean.TRUE.equals(studentClassroomSession.getCascadeOperationToChildren())){
			Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
			for(ClassroomSessionDivision classroomSessionDivision : inject(ClassroomSessionDivisionDao.class).readByClassroomSession(studentClassroomSession.getClassroomSession())){
				StudentClassroomSessionDivision studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionBusiness.class).instanciateOne(classroomSessionDivision, studentClassroomSession.getStudent());
				studentClassroomSessionDivision.setCascadeOperationToChildren(studentClassroomSession.getCascadeOperationToChildren());
				studentClassroomSessionDivision.setCascadeOperationToMaster(studentClassroomSession.getCascadeOperationToMaster());
				studentClassroomSessionDivisions.add(studentClassroomSessionDivision);
			}
			inject(StudentClassroomSessionDivisionBusiness.class).create(studentClassroomSessionDivisions);
			
			if(studentClassroomSession.getClassroomSession().getLevelTimeDivision().getLevel().getLevelName().getAllClassroomSessionDivisionSubjectRequired()){
				Collection<StudentClassroomSessionSubject> studentClassroomSessionSubjects = new ArrayList<>();
				for(ClassroomSessionSubject classroomSessionSubject : inject(ClassroomSessionSubjectDao.class).readByClassroomSession(studentClassroomSession.getClassroomSession())){
					StudentClassroomSessionSubject studentClassroomSessionSubject = inject(StudentClassroomSessionSubjectBusiness.class).instanciateOne(studentClassroomSession,classroomSessionSubject);
					studentClassroomSessionSubject.setCascadeOperationToChildren(studentClassroomSession.getCascadeOperationToChildren());
					//studentClassroomSessionSubject.setCascadeOperationToMaster(studentClassroomSession.getCascadeOperationToMaster());
					//studentClassroomSessionSubject.setCascadeOperationToChildren(Boolean.FALSE);
					studentClassroomSessionSubject.setCascadeOperationToMaster(Boolean.FALSE);
					studentClassroomSessionSubjects.add(studentClassroomSessionSubject);
				}
				inject(StudentClassroomSessionSubjectBusiness.class).create(studentClassroomSessionSubjects);
			}
		}
		
		commonUtils.increment(Integer.class, studentClassroomSession.getClassroomSession().getResults(), NodeResults.FIELD_NUMBER_OF_STUDENT, 1);
	}
	
	@Override
	protected void beforeUpdate(StudentClassroomSession studentClassroomSession) {
		super.beforeUpdate(studentClassroomSession);
		StudentClassroomSession currentStudentClassroomSession = dao.read(studentClassroomSession.getIdentifier());
		if(currentStudentClassroomSession.getClassroomSession().equals(studentClassroomSession.getClassroomSession())){
			
		}else{
			//logTrace("Moving student from classroom session {} to {}", currentStudentClassroomSession.getClassroomSession(),studentClassroomSession.getClassroomSession());
			
		}
		if(studentClassroomSession.getDetailCollection()!=null && studentClassroomSession.getDetailCollection().isSynchonizationEnabled()){
			inject(StudentClassroomSessionDivisionBusiness.class).update(studentClassroomSession.getDetailCollection().getCollection());
		}
	}
	
	@Override
	protected void afterUpdate(StudentClassroomSession studentClassroomSession) {
		super.afterUpdate(studentClassroomSession);
		synchronise(StudentClassroomSessionSubject.class, studentClassroomSession, studentClassroomSession.getStudentClassroomSessionSubjects());
	}
	
	@Override
	protected void beforeDelete(StudentClassroomSession studentClassroomSession) {
		super.beforeDelete(studentClassroomSession);
		inject(StudentClassroomSessionDivisionBusiness.class).delete(inject(StudentClassroomSessionDivisionDao.class)
				.readByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()));
		
		commonUtils.increment(Integer.class, studentClassroomSession.getClassroomSession().getResults(), NodeResults.FIELD_NUMBER_OF_STUDENT, -1);
		
		inject(StudentClassroomSessionSubjectBusiness.class).delete(inject(StudentClassroomSessionSubjectDao.class)
				.readByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()));
	}
		
	/**/
	
	@Override
	public Collection<StudentClassroomSession> updateAverage(Collection<ClassroomSession> classroomSessions,BusinessServiceCallArguments<StudentClassroomSession> callArguments) {
		/*
		 * Data loading
		 */
		Collection<StudentClassroomSession> studentClassroomSessions = dao.readByClassroomSessions(classroomSessions);
		//Collection<ClassroomSessionDivision> classroomSessionDivisions = classroomSessionDivisionDao.readByClassroomSessions(classroomSessions);
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = studentClassroomSessionDivisionDao.readByClassroomSessions(classroomSessions);
		//logTrace("Loaded data. StudentSubjectEvaluation={} , StudentSubject={} , StudentClassroomSessionDivision={}"
		//		,studentSubjectEvaluations.size(),studentSubjects.size(),studentClassroomSessionDivisions.size());
		
		setCallArgumentsObjects(callArguments, studentClassroomSessions);
		/*
		 * Data computing
		 */
		
		updateAverage(classroomSessions, studentClassroomSessions,studentClassroomSessionDivisions, callArguments);
		
		Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>();
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions)
			classroomSessionDivisions.add(studentClassroomSessionDivision.getClassroomSessionDivision());
		
		inject(ClassroomSessionDivisionBusiness.class).computeResults(classroomSessionDivisions, studentClassroomSessionDivisions);
		inject(ClassroomSessionBusiness.class).computeResults(classroomSessions, studentClassroomSessions);
		
		return studentClassroomSessions;
	}
	
	/**/
	
	@Override
	protected Class<StudentClassroomSession> getResultClass() {
		return StudentClassroomSession.class;
	}
	
	@Override
	protected Class<StudentClassroomSessionDivision> getDetailsClass() {
		return StudentClassroomSessionDivision.class;
	}
	
	@Override
	protected WeightedValue weightedValue(StudentClassroomSessionDivision detail) {
		return new WeightedValue(detail.getResults().getEvaluationSort().getAverage().getValue(),detail.getClassroomSessionDivision().getWeight(),Boolean.FALSE);
	}

	@Override
	protected Student student(StudentClassroomSessionDivision detail) {
		return detail.getStudent();
	}
 
	@Override
	protected Collection<StudentClassroomSession> readResults(Collection<ClassroomSession> levels) {
		return dao.readByClassroomSessions(levels);
	}

	@Override
	protected Collection<StudentClassroomSessionDivision> readDetails(Collection<ClassroomSession> levels,Boolean keepDetails) {
		//structure
		Collection<ClassroomSessionDivision> classroomSessionDivisions = classroomSessionDivisionDao.readByClassroomSessions(levels);
		Collection<ClassroomSessionDivisionSubject> subjects = subjectDao.readByClassroomSessions(levels);
		//student data
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = studentClassroomSessionDivisionDao.readByClassroomSessions(levels);
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectDao.readByClassroomSessions(levels);
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> evaluatedStudents = evaluatedStudentDao.readByClassroomSessions(levels);
		
		studentSubjectBusiness.updateAverage(subjects, studentSubjects, evaluatedStudents,null);
		
		studentClassroomSessionDivisionBusiness.updateAverage(classroomSessionDivisions, studentClassroomSessionDivisions, studentSubjects,null);
		
		return studentClassroomSessionDivisions;
	}
	
	@Override
	protected ClassroomSession level(StudentClassroomSession result) {
		return result.getClassroomSession();
	}
	
	@Override
	protected ClassroomSession level(StudentClassroomSessionDivision detail) {
		return detail.getClassroomSessionDivision().getClassroomSession();
	}
	
	@Override
	protected IntervalCollection averageAppreciatedIntervalCollection(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getNodeInformations().getStudentClassroomSessionAverageScale();
	}
	
	@Override
	protected IntervalCollection averagePromotedIntervalCollection(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getNodeInformations().getStudentClassroomSessionAveragePromotionScale();
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return dao.readByClassroomSessions(classroomSessions);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByAcademicSession(AcademicSession academicSession) {
		return dao.readByAcademicSession(academicSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public StudentClassroomSession findByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student,classroomSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return dao.readByLevelTimeDivision(levelTimeDivision);
	}

	/**/
	
	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSession> levels) {
		return lectureDao.readByClassroomSessions(levels);
	}

	@Override
	protected ClassroomSession level(Lecture lecture) {
		return lecture.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession();
	}
	
	@Override
	protected Boolean isLectureAttendanceAggregatable(StudentClassroomSession studentClassroomSession) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	protected Long getAttendableDuration(StudentClassroomSession studentClassroomSession) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSession> findByCriteria(SearchCriteria criteria) {
		prepareFindByCriteria(criteria);
		return dao.readByCriteria(criteria);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Long countByCriteria(SearchCriteria criteria) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public StudentClassroomSession instanciateOne(String[] values) {
		StudentClassroomSession studentClassroomSession = instanciateOne();
		Integer index = 0;
		studentClassroomSession.setStudent(inject(StudentDao.class).read(values[index++]));
		studentClassroomSession.setClassroomSession(inject(ClassroomSessionDao.class).read(values[index++]));
		return studentClassroomSession;
	}
	
	/**/
	
	

}
