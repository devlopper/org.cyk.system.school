package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Student.SearchCriteria;
import org.cyk.system.school.persistence.api.actor.StudentDao;

@Stateless
public class StudentBusinessImpl extends AbstractActorBusinessImpl<Student, StudentDao,SearchCriteria> implements StudentBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentBusinessImpl(StudentDao dao) {
		super(dao);  
	}



    
}
