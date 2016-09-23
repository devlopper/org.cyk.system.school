package org.cyk.system.school.business.api.actor;

import java.util.Collection;

import org.cyk.system.root.business.api.party.person.AbstractActorBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionDivision;

public interface StudentBusiness extends AbstractActorBusiness<Student,SearchCriteria> {

	Collection<Student> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	
}
