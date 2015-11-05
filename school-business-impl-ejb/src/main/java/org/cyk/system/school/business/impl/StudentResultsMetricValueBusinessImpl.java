package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.persistence.api.StudentResultsMetricValueDao;

public class StudentResultsMetricValueBusinessImpl extends AbstractTypedBusinessService<StudentResultsMetricValue, StudentResultsMetricValueDao> implements StudentResultsMetricValueBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentResultsMetricValueBusinessImpl(StudentResultsMetricValueDao dao) {
		super(dao); 
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<StudentResultsMetricValue> findByStudentResults(StudentResults studentResults) {
		return dao.readByStudentResults(studentResults);
	}
	

	
}
