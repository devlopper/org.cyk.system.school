package org.cyk.system.school.persistence.impl;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.persistence.api.StudentResultsDao;

public class StudentResultsDaoImpl extends AbstractTypedDao<StudentResults> implements StudentResultsDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByMetricCollection;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByMetricCollection, "SELECT r FROM StudentResults r WHERE EXISTS "
				+ "(SELECT r1 FROM StudentResultsMetricValue r1 WHERE r1.studentResults = r AND r1.metricValue.metric.collection = :collection)");
	}
	
	@Override
	public Collection<StudentResults> readByMetricCollection(MetricCollection metricCollection) {
		return namedQuery(readByMetricCollection).parameter(Metric.FIELD_COLLECTION, metricCollection).resultMany();
	}

	
}
 