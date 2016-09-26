package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.MetricCollection;
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
	
	@Override
	protected Object[] getPropertyValueTokens(StudentResultsMetricValue studentResultsMetricValue, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{studentResultsMetricValue.getStudentResults(),studentResultsMetricValue.getMetricValue()};
		return super.getPropertyValueTokens(studentResultsMetricValue, name);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentResultsMetricValue> findByStudentResults(StudentResults studentResults) {
		return dao.readByStudentResults(studentResults);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentResultsMetricValue> findByStudentResultsByMetricCollection(StudentResults studentResults,MetricCollection metricCollection) {
		return dao.readByStudentResultsByMetricCollection(studentResults,metricCollection);
	}
	
}
