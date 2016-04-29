package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.actor.Teacher.SearchCriteria;
import org.cyk.system.school.persistence.api.actor.TeacherDao;

@Stateless
public class TeacherBusinessImpl extends AbstractActorBusinessImpl<Teacher, TeacherDao,SearchCriteria> implements TeacherBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public TeacherBusinessImpl(TeacherDao dao) {
		super(dao);  
	}



    
}
