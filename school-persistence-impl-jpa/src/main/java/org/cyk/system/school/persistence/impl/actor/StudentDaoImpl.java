package org.cyk.system.school.persistence.impl.actor;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.party.person.AbstractActorDaoImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.actor.StudentDao;

public class StudentDaoImpl extends AbstractActorDaoImpl<Student,SearchCriteria> implements StudentDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivision;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivision, "SELECT record FROM Student record WHERE EXISTS("
				+ "SELECT studentClassroomSessionDivision FROM StudentClassroomSessionDivision studentClassroomSessionDivision "
				+ "WHERE studentClassroomSessionDivision.student = record AND studentClassroomSessionDivision.classroomSessionDivision = :classroomSessionDivision)");
	}
	
	@Override
	public Collection<Student> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.resultMany();
	}

	
	
}
  