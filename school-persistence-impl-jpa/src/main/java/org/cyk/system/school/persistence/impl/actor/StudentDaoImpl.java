package org.cyk.system.school.persistence.impl.actor;

import java.io.Serializable;

import org.cyk.system.root.persistence.impl.party.person.AbstractActorDaoImpl;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.persistence.api.actor.StudentDao;

public class StudentDaoImpl extends AbstractActorDaoImpl<Student,SearchCriteria> implements StudentDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	
	
}
  