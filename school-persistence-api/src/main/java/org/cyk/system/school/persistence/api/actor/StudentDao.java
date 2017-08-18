package org.cyk.system.school.persistence.api.actor;

import java.util.Collection;

import org.cyk.system.root.persistence.api.party.person.AbstractActorDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface StudentDao extends AbstractActorDao<Student,SearchCriteria> {

	Collection<Student> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<Student> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	Collection<Student> readNotByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
}
 