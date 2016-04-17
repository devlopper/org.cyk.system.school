package org.cyk.system.school.business.impl;

import java.io.Serializable;

import org.cyk.system.company.business.impl.adapter.ActorBusinessServiceAdapter;
import org.cyk.system.school.model.actor.Student;

public class StudentBusinessServiceAdapter extends ActorBusinessServiceAdapter<Student> implements Serializable {

	private static final long serialVersionUID = -8384047237353201461L;
	
	public static Boolean IS_CUSTOMER = Boolean.FALSE;
	
	@Override
	protected Boolean isCustomer() {
		return IS_CUSTOMER;
	}
}
