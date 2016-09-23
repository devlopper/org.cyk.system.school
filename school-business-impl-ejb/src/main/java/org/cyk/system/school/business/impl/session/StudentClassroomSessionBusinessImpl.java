package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.company.business.api.sale.SaleBusiness;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession.SearchCriteria;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;

@Stateless
public class StudentClassroomSessionBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSession, StudentClassroomSession, StudentClassroomSessionDao, StudentClassroomSessionDivision> implements StudentClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionDivisionSubjectBusiness studentSubjectBusiness;
	
	@Inject private StudentClassroomSessionDivisionSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao;
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
	@Inject private ClassroomSessionDao classroomSessionDao;
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public StudentClassroomSessionBusinessImpl(StudentClassroomSessionDao dao) {
		super(dao); 
	}
	
	@Override
	public StudentClassroomSession create(StudentClassroomSession studentClassroomSession) {
		super.create(studentClassroomSession);
		logInstanceCreated(studentClassroomSession);
		//logTrace("Student {} for classroomsession {} registered", studentClassroomSession.getStudent(),studentClassroomSession.getClassroomSession());
		
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
		if(Boolean.TRUE.equals(studentClassroomSession.getCascadeOperationToChildren())){
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisionDao.readByClassroomSession(studentClassroomSession.getClassroomSession()))
				studentClassroomSessionDivisions.add(new StudentClassroomSessionDivision(studentClassroomSession.getStudent(), classroomSessionDivision));
		}
		cascade(studentClassroomSession, studentClassroomSessionDivisions, Crud.CREATE);
		
		return studentClassroomSession;
	}
	
	private void cascade(StudentClassroomSession studentClassroomSession,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions,Crud crud){
		new CascadeOperationListener.Adapter.Default<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDao,StudentClassroomSessionDivisionBusiness>(null
				,inject(StudentClassroomSessionDivisionBusiness.class))
			.operate(studentClassroomSessionDivisions, crud);
		commonUtils.increment(Long.class, studentClassroomSession.getClassroomSession(), ClassroomSession.FIELD_NUMBER_OF_STUDENTS
				, Crud.CREATE.equals(crud)?1l:Crud.DELETE.equals(crud)?-1l:0l);
		classroomSessionDao.update(studentClassroomSession.getClassroomSession());
	}
	
	@Override
	public StudentClassroomSession update(StudentClassroomSession studentClassroomSession) {
		StudentClassroomSession currentStudentClassroomSession = dao.read(studentClassroomSession.getIdentifier());
		if(currentStudentClassroomSession.getClassroomSession().equals(studentClassroomSession.getClassroomSession())){
			
		}else{
			//logTrace("Moving student from classroom session {} to {}", currentStudentClassroomSession.getClassroomSession(),studentClassroomSession.getClassroomSession());
			
		}
	
		if(studentClassroomSession.getTuitionSale()!=null && studentClassroomSession.getTuitionSale().getIdentifier()==null){
			inject(SaleBusiness.class).create(studentClassroomSession.getTuitionSale());
		}
		return super.update(studentClassroomSession);
	}
	
	@Override
	public StudentClassroomSession delete(StudentClassroomSession studentClassroomSession) {
		cascade(studentClassroomSession, studentClassroomSessionDivisionDao.readByStudentByClassroomSession(studentClassroomSession.getStudent()
				, studentClassroomSession.getClassroomSession()), Crud.DELETE);
		return super.delete(studentClassroomSession);
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
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
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

	/**/
	
	

}
