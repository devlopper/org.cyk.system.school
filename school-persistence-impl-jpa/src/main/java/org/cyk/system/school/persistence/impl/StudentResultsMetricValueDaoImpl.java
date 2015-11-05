package org.cyk.system.school.persistence.impl;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.persistence.api.StudentResultsMetricValueDao;

public class StudentResultsMetricValueDaoImpl extends AbstractTypedDao<StudentResultsMetricValue> implements StudentResultsMetricValueDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByStudentResults;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByStudentResults, _select().where(StudentResultsMetricValue.FIELD_STUDENT_RESULTS));
	}
	
	@Override
	public Collection<StudentResultsMetricValue> readByStudentResults(StudentResults studentResults) {
		return namedQuery(readByStudentResults).parameter(StudentResultsMetricValue.FIELD_STUDENT_RESULTS, studentResults).resultMany();
	}
	
}
 