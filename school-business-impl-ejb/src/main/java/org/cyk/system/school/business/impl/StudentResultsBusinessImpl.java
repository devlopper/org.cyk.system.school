package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.StudentResultsBusiness;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.persistence.api.StudentResultsDao;

public class StudentResultsBusinessImpl extends AbstractTypedBusinessService<StudentResults, StudentResultsDao> implements StudentResultsBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	/*
	@Override
	protected Object[] getPropertyValueTokens(StudentResults studentResults, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{studentResults.get,studentResults.getMetricValue()};
		return super.getPropertyValueTokens(studentResults, name);
	}
	*/
	
	@Inject
	public StudentResultsBusinessImpl(StudentResultsDao dao) {
		super(dao); 
	}
	
}
