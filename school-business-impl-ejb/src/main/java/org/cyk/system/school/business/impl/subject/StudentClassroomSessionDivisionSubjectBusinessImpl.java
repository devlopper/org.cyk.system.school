package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;

@Stateless
public class StudentClassroomSessionDivisionSubjectBusinessImpl extends AbstractStudentResultsBusinessImpl<StudentClassroomSessionDivisionSubject, StudentClassroomSessionDivisionSubjectDao,ClassroomSessionDivisionSubject, StudentClassroomSessionDivisionSubjectEvaluation> implements StudentClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
	@Inject private StudentClassroomSessionDivisionSubjectEvaluationDao studentSubjectEvaluationDao;
	
	@Inject
	public StudentClassroomSessionDivisionSubjectBusinessImpl(StudentClassroomSessionDivisionSubjectDao dao) {
		super(dao); 
	}
	
	@Override
	protected void beforeCreate(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject) {
		super.beforeCreate(studentClassroomSessionDivisionSubject);
		exceptionUtils().exists(dao.readByStudentByClassroomSessionDivisionBySubject(studentClassroomSessionDivisionSubject.getStudent()
				, studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision()
				, studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject().getSubject()));
	}
	
	@Override
	protected void afterCreate(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject) {
		super.afterCreate(studentClassroomSessionDivisionSubject);
		Student student = studentClassroomSessionDivisionSubject.getStudent();
		ClassroomSessionDivision classroomSessionDivision = studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision();
		
		if(Boolean.TRUE.equals(studentClassroomSessionDivisionSubject.getCascadeOperationToMaster())){
			StudentClassroomSessionDivision studentClassroomSessionDivision = studentClassroomSessionDivisionDao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
			if(studentClassroomSessionDivision==null){
				studentClassroomSessionDivision = new StudentClassroomSessionDivision(student, classroomSessionDivision);
				studentClassroomSessionDivision.setCascadeOperationToChildren(studentClassroomSessionDivisionSubject.getCascadeOperationToChildren());
				studentClassroomSessionDivision.setCascadeOperationToMaster(studentClassroomSessionDivisionSubject.getCascadeOperationToMaster());
				inject(StudentClassroomSessionDivisionBusiness.class).create(studentClassroomSessionDivision);
			}
		}
		cascade(studentClassroomSessionDivisionSubject,Crud.CREATE);
	}
	
	private void cascade(StudentClassroomSessionDivisionSubject studentSubject,Crud crud){
		
	}

	@Override
	public StudentClassroomSessionDivisionSubject delete(StudentClassroomSessionDivisionSubject studentSubject) {
		cascade(studentSubject,Crud.DELETE);
		for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluationDao.readByStudentSubject(studentSubject))
			studentSubjectEvaluationDao.delete(studentSubjectEvaluation);
		return super.delete(studentSubject);
	}
		 
	/**/
	
	@Override
	protected Class<StudentClassroomSessionDivisionSubject> getResultClass() {
		return StudentClassroomSessionDivisionSubject.class;
	}
	@Override
	protected Class<StudentClassroomSessionDivisionSubjectEvaluation> getDetailsClass() {
		return StudentClassroomSessionDivisionSubjectEvaluation.class;
	}
	
	@Override
	protected WeightedValue weightedValue(StudentClassroomSessionDivisionSubjectEvaluation detail) {
		return new WeightedValue(detail.getValue(), detail.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getWeight()
				,Boolean.TRUE.equals(detail.getEvaluation().getCoefficientApplied()));
	}

	@Override
	protected Student student(StudentClassroomSessionDivisionSubjectEvaluation detail) {
		return detail.getStudentClassroomSessionDivisionSubject().getStudent();
	}

	@Override
	protected Collection<StudentClassroomSessionDivisionSubject> readResults(Collection<ClassroomSessionDivisionSubject> levels) {
		return dao.readBySubjects(levels); 
	}

	@Override
	protected Collection<StudentClassroomSessionDivisionSubjectEvaluation> readDetails(Collection<ClassroomSessionDivisionSubject> levels,Boolean keepDetails) {
		return evaluatedStudentDao.readByClassroomSessionDivisionSubjects(levels);
	}
	 
	@Override
	protected ClassroomSessionDivisionSubject level(StudentClassroomSessionDivisionSubjectEvaluation detail) {
		return detail.getStudentClassroomSessionDivisionSubject().getClassroomSessionDivisionSubject();
	}

	@Override
	protected ClassroomSessionDivisionSubject level(StudentClassroomSessionDivisionSubject result) {
		return result.getClassroomSessionDivisionSubject();
	}
	
	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSessionDivisionSubject> levels) {
		return lectureDao.readBySubjects(levels);
	}

	@Override
	protected ClassroomSessionDivisionSubject level(Lecture lecture) {
		return lecture.getClassroomSessionDivisionSubject();
	}
	
	@Override
	protected IntervalCollection averageAppreciatedIntervalCollection(ClassroomSessionDivisionSubject subject) {
		return inject(ClassroomSessionBusiness.class).findCommonNodeInformations(subject.getClassroomSessionDivision().getClassroomSession()).getStudentSubjectAverageScale();
	}
	
	@Override
	protected IntervalCollection averagePromotedIntervalCollection(ClassroomSessionDivisionSubject subject) {
		return inject(ClassroomSessionBusiness.class).findCommonNodeInformations(subject.getClassroomSessionDivision().getClassroomSession()).getStudentClassroomSessionAveragePromotionScale();
	}
	
	@Override
	protected Boolean isLectureAttendanceAggregatable(StudentClassroomSessionDivisionSubject studentSubject) {
		return null;
	}
	
	@Override
	protected Long getAttendableDuration(StudentClassroomSessionDivisionSubject studentSubject) {
		return null;
	}
	
	/**/
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject) {
		return dao.readByClassroomSessionDivisionSubject(subject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentClassroomSessionDivisionSubject findByStudentByClassroomSessionDivisionSubject(Student student,ClassroomSessionDivisionSubject subject) {
		return dao.readByStudentBySubject(student, subject);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByStudent(Student student) {
		return dao.readByStudent(student);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student, classroomSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision,Teacher teacher) {
		return dao.readByClassroomSessionDivisionByTeacher(classroomSessionDivision,teacher);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentClassroomSessionDivisionSubject findByStudentByClassroomSessionDivisionBySubject(Student student,ClassroomSessionDivision classroomSessionDivision, Subject subject) {
		return dao.readByStudentByClassroomSessionDivisionBySubject(student,classroomSessionDivision,subject);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return dao.readByClassroomSessions(classroomSessions);
	}
	 
	/**/
	
}
