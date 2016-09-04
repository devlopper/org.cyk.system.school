package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.StudentResultsBusiness;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.persistence.api.StudentResultsDao;

public class StudentResultsBusinessImpl extends AbstractTypedBusinessService<StudentResults, StudentResultsDao> implements StudentResultsBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentResultsBusinessImpl(StudentResultsDao dao) {
		super(dao); 
	}
	
}
