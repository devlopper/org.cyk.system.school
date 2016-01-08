package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractStudentResultsBusinessImpl;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;

@Stateless
public class StudentClassroomSessionBusinessImpl extends AbstractStudentResultsBusinessImpl<ClassroomSession, StudentClassroomSession, StudentClassroomSessionDao, StudentClassroomSessionDivision> implements StudentClassroomSessionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectDao subjectDao;
	@Inject private StudentClassroomSessionDivisionDao studentClassroomSessionDivisionDao;
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
		if(Boolean.TRUE.equals(studentClassroomSession.getCascadeTopDownOnCreate())){
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisionDao.readByClassroomSession(studentClassroomSession.getClassroomSession()))
				studentClassroomSessionDivisions.add(new StudentClassroomSessionDivision(studentClassroomSession.getStudent(), classroomSessionDivision));
		}
		cascade(studentClassroomSession, studentClassroomSessionDivisions, Crud.CREATE);
		return studentClassroomSession;
	}
	
	private void cascade(StudentClassroomSession studentClassroomSession,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions,Crud crud){
		new CascadeOperationListener.Adapter.Default<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDao,StudentClassroomSessionDivisionBusiness>(null
				,SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness())
			.operate(studentClassroomSessionDivisions, crud);
	}
	
	@Override
	public StudentClassroomSession update(StudentClassroomSession studentClassroomSession) {
		StudentClassroomSession currentStudentClassroomSession = dao.read(studentClassroomSession.getIdentifier());
		if(currentStudentClassroomSession.getClassroomSession().equals(studentClassroomSession.getClassroomSession())){
			
		}else{
			//logTrace("Moving student from classroom session {} to {}", currentStudentClassroomSession.getClassroomSession(),studentClassroomSession.getClassroomSession());
			
		}
		return super.update(studentClassroomSession);
	}
	
	@Override
	public StudentClassroomSession delete(StudentClassroomSession studentClassroomSession) {
		cascade(studentClassroomSession, studentClassroomSessionDivisionDao.readByStudentByClassroomSession(studentClassroomSession.getStudent()
				, studentClassroomSession.getClassroomSession()), Crud.DELETE);
		return super.delete(studentClassroomSession);
	}
	
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
		return new WeightedValue(detail.getResults().getEvaluationSort().getAverage().getValue(),detail.getClassroomSessionDivision().getCoefficient(),Boolean.FALSE);
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
		Collection<StudentSubject> studentSubjects = studentSubjectDao.readByClassroomSessions(levels);
		Collection<StudentSubjectEvaluation> evaluatedStudents = evaluatedStudentDao.readByClassroomSessions(levels);
		
		studentSubjectBusiness.average(subjects, studentSubjects, evaluatedStudents,keepDetails);
		
		studentClassroomSessionDivisionBusiness.average(classroomSessionDivisions, studentClassroomSessionDivisions, studentSubjects,keepDetails);
		
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
	protected IntervalCollection averageIntervalCollection(ClassroomSession classroomSession) {
		return classroomSession.getLevelTimeDivision().getLevel().getName().getNodeInformations().getStudentClassroomSessionAverageScale();
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<StudentClassroomSession> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public StudentClassroomSession finddByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student, classroomSession);
	}

	/**/
	/*
    @AllArgsConstructor
	private class StudentClassroomSessionDivisionWeight implements Weightable,Serializable {

		private static final long serialVersionUID = -7151566926896987903L;
		
		private StudentClassroomSessionDivision studentClassroomSessionDivision;
		
		@Override
		public BigDecimal getValue() {
			if(studentClassroomSessionDivision.getResults().getAverage()==null)
				return null;
			return studentClassroomSessionDivision.getResults().getAverage().multiply(studentClassroomSessionDivision.getClassroomSessionDivision().getCoefficient());
		}

		@Override
		public BigDecimal getWeight() {
			return studentClassroomSessionDivision.getClassroomSessionDivision().getCoefficient();
		}

	}
	*/

	@Override
	protected Collection<Lecture> readLectures(Collection<ClassroomSession> levels) {
		return lectureDao.readByClassroomSessions(levels);
	}

	@Override
	protected ClassroomSession level(Lecture lecture) {
		return lecture.getSubject().getClassroomSessionDivision().getClassroomSession();
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public StudentClassroomSession findByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return dao.readByStudentByClassroomSession(student,classroomSession);
	}

	/**/
	
	

}
