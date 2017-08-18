package org.cyk.system.school.persistence.impl.actor;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.party.person.AbstractActorDaoImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;

public class StudentDaoImpl extends AbstractActorDaoImpl<Student,SearchCriteria> implements StudentDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivision,readByClassroomSessionDivisionSubject,readNotByClassroomSessionDivisionSubject;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivision, "SELECT record FROM Student record WHERE EXISTS("
				+ "SELECT studentClassroomSessionDivision FROM StudentClassroomSessionDivision studentClassroomSessionDivision "
				+ "WHERE studentClassroomSessionDivision.student = record AND studentClassroomSessionDivision.classroomSessionDivision = :classroomSessionDivision)");
		
		registerNamedQuery(readByClassroomSessionDivisionSubject, "SELECT record FROM Student record WHERE EXISTS("
				+ "SELECT studentClassroomSessionDivisionSubject FROM StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject "
				+ "WHERE studentClassroomSessionDivisionSubject.student = record AND studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject = :classroomSessionDivisionSubject)");
		
		registerNamedQuery(readNotByClassroomSessionDivisionSubject, "SELECT record FROM Student record WHERE NOT EXISTS("
				+ "SELECT studentClassroomSessionDivisionSubject FROM StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject "
				+ "WHERE studentClassroomSessionDivisionSubject.student = record AND studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject = :classroomSessionDivisionSubject)");
	}
	
	@Override
	public Collection<Student> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.resultMany();
	}

	@Override
	public Collection<Student> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivision).parameter(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject)
				.resultMany();
	}

	@Override
	public Collection<Student> readNotByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readNotByClassroomSessionDivisionSubject).parameter(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject)
				.resultMany();
	}
	
}
  