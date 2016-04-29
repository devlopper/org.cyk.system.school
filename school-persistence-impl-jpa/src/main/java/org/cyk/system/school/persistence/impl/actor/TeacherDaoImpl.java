package org.cyk.system.school.persistence.impl.actor;

import java.io.Serializable;

import org.cyk.system.root.persistence.impl.party.person.AbstractActorDaoImpl;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.actor.Teacher.SearchCriteria;
import org.cyk.system.school.persistence.api.actor.TeacherDao;

public class TeacherDaoImpl extends AbstractActorDaoImpl<Teacher,SearchCriteria> implements TeacherDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
	
}
  