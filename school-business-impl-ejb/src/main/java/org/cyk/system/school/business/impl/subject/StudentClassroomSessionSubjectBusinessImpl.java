package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionSubjectDao;

public class StudentClassroomSessionSubjectBusinessImpl extends AbstractStudentResultsBusinessImpl<StudentClassroomSessionSubject, StudentClassroomSessionSubjectDao,ClassroomSessionSubject, StudentClassroomSessionDivisionSubject> implements StudentClassroomSessionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentClassroomSessionSubjectBusinessImpl(StudentClassroomSessionSubjectDao dao) {
		super(dao); 
	}

	@Override
	public StudentClassroomSessionSubject instanciateOne(StudentClassroomSession studentClassroomSession,ClassroomSessionSubject classroomSessionSubject) {
		StudentClassroomSessionSubject studentClassroomSessionSubject = instanciateOne();
		studentClassroomSessionSubject.setStudent(studentClassroomSession.getStudent());
		studentClassroomSessionSubject.setClassroomSessionSubject(classroomSessionSubject);
		return studentClassroomSessionSubject;
	}
	
	@Override
	public Collection<StudentClassroomSessionSubject> findByStudentClassroomSession(StudentClassroomSession studentClassroomSession) {
		return dao.readByStudentByClassroomSession(studentClassroomSession.getStudent(),studentClassroomSession.getClassroomSession());
	}

	@Override
	protected Class<StudentClassroomSessionDivisionSubject> getDetailsClass() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Class<StudentClassroomSessionSubject> getResultClass() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected WeightedValue weightedValue(StudentClassroomSessionDivisionSubject detail) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Student student(StudentClassroomSessionDivisionSubject detail) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Collection<StudentClassroomSessionSubject> readResults(Collection<ClassroomSessionSubject> levels) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSessionSubject> levels) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Collection<StudentClassroomSessionDivisionSubject> readDetails(Collection<ClassroomSessionSubject> levels, Boolean keepDetails) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected ClassroomSessionSubject level(StudentClassroomSessionDivisionSubject detail) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected ClassroomSessionSubject level(StudentClassroomSessionSubject result) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Boolean isLectureAttendanceAggregatable(StudentClassroomSessionSubject result) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Long getAttendableDuration(StudentClassroomSessionSubject result) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected ClassroomSessionSubject level(Lecture lecture) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected IntervalCollection averageAppreciatedIntervalCollection(ClassroomSessionSubject level) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected IntervalCollection averagePromotedIntervalCollection(ClassroomSessionSubject level) {
		// TODO Auto-generated method stub
		return null;
	}
	
}
