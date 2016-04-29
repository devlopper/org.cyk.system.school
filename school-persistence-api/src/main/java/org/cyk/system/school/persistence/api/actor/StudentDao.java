package org.cyk.system.school.persistence.api.actor;

import org.cyk.system.root.persistence.api.party.person.AbstractActorDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;

public interface StudentDao extends AbstractActorDao<Student,SearchCriteria> {
	
}
 