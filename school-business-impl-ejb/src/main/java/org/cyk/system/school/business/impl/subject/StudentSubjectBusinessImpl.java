package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;

@Stateless
public class StudentSubjectBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSessionDivisionSubject, StudentSubject, StudentSubjectDao, StudentSubjectEvaluation> implements StudentSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
	
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
		
		StudentClassroomSessionDivision studentClassroomSessionDivision = studentClassroomSessionDivisionDao.readByStudentByClassroomSessionDivision(student, classroomSessionDivision);
		if(studentClassroomSessionDivision==null){
			schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().create(new StudentClassroomSessionDivision(student, classroomSessionDivision));
		}
		logTrace("Student {} for subject {} registered", studentSubject.getStudent(),studentSubject.getClassroomSessionDivisionSubject());
		return studentSubject;
	}
		 
	/**/
	
	@Override
	protected WeightedValue weightedValue(StudentSubjectEvaluation detail) {
		return new WeightedValue(detail.getValue(), detail.getSubjectEvaluation().getType().getCoefficient()
				, /*Boolean.TRUE*/ Boolean.TRUE.equals(detail.getSubjectEvaluation().getCoefficientApplied()));
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
		return evaluatedStudentDao.readBySubjects(levels);
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
		return lecture.getSubject();
	}
	
	@Override
	protected IntervalCollection averageIntervalCollection(ClassroomSessionDivisionSubject subject) {
		return subject.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentSubjectAverageScale();
	}
	
	/**/
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<StudentSubject> findBySubject(ClassroomSessionDivisionSubject subject) {
		return dao.readByClassroomSessionDivisionSubject(subject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public StudentSubject findByStudentBySubject(Student student,ClassroomSessionDivisionSubject subject) {
		return dao.readByStudentBySubject(student, subject);
	}
	 
	/**/
	
}
