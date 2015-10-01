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
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;

@Stateless
public class StudentSubjectBusinessImpl extends AbstractStudentResultsBusinessImpl<Subject, StudentSubject, StudentSubjectDao, EvaluatedStudent> implements StudentSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentSubjectBusinessImpl(StudentSubjectDao dao) {
		super(dao); 
	}
		 
	/**/
	
	@Override
	protected WeightedValue weightedValue(EvaluatedStudent detail) {
		return new WeightedValue(detail.getValue(), detail.getEvaluation().getType().getCoefficient(), Boolean.TRUE);
	}

	@Override
	protected Student student(EvaluatedStudent detail) {
		return detail.getStudentSubject().getStudent();
	}

	@Override
	protected Collection<StudentSubject> readResults(Collection<Subject> levels) {
		return dao.readBySubjects(levels); 
	}

	@Override
	protected Collection<EvaluatedStudent> readDetails(Collection<Subject> levels,Boolean keepDetails) {
		return evaluatedStudentDao.readBySubjects(levels);
	}
	 
	@Override
	protected Subject level(EvaluatedStudent detail) {
		return detail.getStudentSubject().getSubject();
	}

	@Override
	protected Subject level(StudentSubject result) {
		return result.getSubject();
	}
	
	@Override
	protected Collection<Lecture> readLectures(Collection<Subject> levels) {
		return lectureDao.readBySubjects(levels);
	}

	@Override
	protected Subject level(Lecture lecture) {
		return lecture.getSubject();
	}
	
	@Override
	protected IntervalCollection averageIntervalCollection(Subject subject) {
		return subject.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentSubjectAverageScale();
	}
	
	/**/
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubject> findBySubject(Subject subject) {
		return dao.readBySubject(subject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public StudentSubject findByStudentBySubject(Student student,Subject subject) {
		return dao.readByStudentBySubject(student, subject);
	}
	 
	/**/
	
}
