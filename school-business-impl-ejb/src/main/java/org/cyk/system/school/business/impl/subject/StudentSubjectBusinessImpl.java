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
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;

@Stateless
public class StudentSubjectBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSessionDivisionSubject, StudentSubject, StudentSubjectDao, StudentSubjectEvaluation> implements StudentSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
	@Inject private StudentSubjectEvaluationDao studentSubjectEvaluationDao;
	
	@Inject
	public StudentSubjectBusinessImpl(StudentSubjectDao dao) {
		super(dao); 
	}
	
	@Override
	public StudentSubject create(StudentSubject studentSubject) {
		studentSubject = super.create(studentSubject);
		Student student = studentSubject.getStudent();
		ClassroomSessionDivision classroomSessionDivision = studentSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision();
		if(studentSubject.getResults()==null)
			studentSubject.setResults(new StudentResults());
		
		if(Boolean.TRUE.equals(studentSubject.getCascadeBottomUpOnCreate())){
			StudentClassroomSessionDivision studentClassroomSessionDivision = studentClassroomSessionDivisionDao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
			if(studentClassroomSessionDivision==null){
				studentClassroomSessionDivision = new StudentClassroomSessionDivision(student, classroomSessionDivision);
				studentClassroomSessionDivision.setCascadeTopDownOnCreate(studentSubject.getCascadeTopDownOnCreate());
				studentClassroomSessionDivision.setCascadeBottomUpOnCreate(studentSubject.getCascadeBottomUpOnCreate());
				schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().create(studentClassroomSessionDivision);
			}
		}
		cascade(studentSubject,Crud.CREATE);
		logInstanceCreated(studentSubject);
		//logTrace("Student {} for subject {} registered", studentSubject.getStudent(),studentSubject.getClassroomSessionDivisionSubject());
		return studentSubject;
	}
	
	private void cascade(StudentSubject studentSubject,Crud crud){
		
	}

	@Override
	public StudentSubject delete(StudentSubject studentSubject) {
		cascade(studentSubject,Crud.DELETE);
		for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluationDao.readByStudentSubject(studentSubject))
			studentSubjectEvaluationDao.delete(studentSubjectEvaluation);
		return super.delete(studentSubject);
	}
		 
	/**/
	
	@Override
	protected Class<StudentSubject> getResultClass() {
		return StudentSubject.class;
	}
	@Override
	protected Class<StudentSubjectEvaluation> getDetailsClass() {
		return StudentSubjectEvaluation.class;
	}
	
	@Override
	protected WeightedValue weightedValue(StudentSubjectEvaluation detail) {
		return new WeightedValue(detail.getValue(), detail.getEvaluation().getClassroomSessionDivisionSubjectEvaluationType().getCoefficient()
				,Boolean.TRUE.equals(detail.getEvaluation().getCoefficientApplied()));
	}

	@Override
	protected Student student(StudentSubjectEvaluation detail) {
		return detail.getStudentSubject().getStudent();
	}

	@Override
	protected Collection<StudentSubject> readResults(Collection<ClassroomSessionDivisionSubject> levels) {
		return dao.readBySubjects(levels); 
	}

	@Override
	protected Collection<StudentSubjectEvaluation> readDetails(Collection<ClassroomSessionDivisionSubject> levels,Boolean keepDetails) {
		return evaluatedStudentDao.readByClassroomSessionDivisionSubjects(levels);
	}
	 
	@Override
	protected ClassroomSessionDivisionSubject level(StudentSubjectEvaluation detail) {
		return detail.getStudentSubject().getClassroomSessionDivisionSubject();
	}

	@Override
	protected ClassroomSessionDivisionSubject level(StudentSubject result) {
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
	protected IntervalCollection averageIntervalCollection(ClassroomSessionDivisionSubject subject) {
		return subject.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentSubjectAverageScale();
	}
	
	@Override
	protected Boolean isLectureAttendanceAggregatable(StudentSubject studentSubject) {
		return null;
	}
	
	@Override
	protected Long getAttendableDuration(StudentSubject studentSubject) {
		return null;
	}
	
	/**/
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubject> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject) {
		return dao.readByClassroomSessionDivisionSubject(subject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentSubject findByStudentByClassroomSessionDivisionSubject(Student student,ClassroomSessionDivisionSubject subject) {
		return dao.readByStudentBySubject(student, subject);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubject> findByStudent(Student student) {
		return dao.readByStudent(student);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubject> findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubject> findByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student, classroomSession);
	}
	 
	/**/
	
}
