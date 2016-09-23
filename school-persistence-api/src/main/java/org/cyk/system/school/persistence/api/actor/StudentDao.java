package org.cyk.system.school.persistence.api.actor;

import java.util.Collection;

import org.cyk.system.root.persistence.api.party.person.AbstractActorDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;

public interface StudentDao extends AbstractActorDao<Student,SearchCriteria> {

	Collection<Student> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	
}
 