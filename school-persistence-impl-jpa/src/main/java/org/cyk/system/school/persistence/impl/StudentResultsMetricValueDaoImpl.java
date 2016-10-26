package org.cyk.system.school.persistence.impl;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.persistence.api.StudentResultsMetricValueDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class StudentResultsMetricValueDaoImpl extends AbstractTypedDao<StudentResultsMetricValue> implements StudentResultsMetricValueDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByStudentResults,readByStudentResultsByMetricCollection,readByMetric;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByStudentResults, _select().where(StudentResultsMetricValue.FIELD_STUDENT_RESULTS));
		registerNamedQuery(readByMetric, _select().where(commonUtils.attributePath(StudentResultsMetricValue.FIELD_METRIC_VALUE,MetricValue.FIELD_METRIC),MetricValue.FIELD_METRIC));
		registerNamedQuery(readByStudentResultsByMetricCollection, _select().where(StudentResultsMetricValue.FIELD_STUDENT_RESULTS)
				.and(commonUtils.attributePath(StudentResultsMetricValue.FIELD_METRIC_VALUE, MetricValue.FIELD_METRIC,Metric.FIELD_COLLECTION)
						, Metric.FIELD_COLLECTION,ArithmeticOperator.EQ));
	}
	
	@Override
	public Collection<StudentResultsMetricValue> readByStudentResults(StudentResults studentResults) {
		return namedQuery(readByStudentResults).parameter(StudentResultsMetricValue.FIELD_STUDENT_RESULTS, studentResults).resultMany();
	}
	
	@Override
	public Collection<StudentResultsMetricValue> readByMetric(Metric metric) {
		return namedQuery(readByMetric).parameter(MetricValue.FIELD_METRIC, metric).resultMany();
	}

	@Override
	public Collection<StudentResultsMetricValue> readByStudentResultsByMetricCollection(StudentResults studentResults,MetricCollection metricCollection) {
		return namedQuery(readByStudentResultsByMetricCollection).parameter(StudentResultsMetricValue.FIELD_STUDENT_RESULTS, studentResults)
				.parameter(Metric.FIELD_COLLECTION, metricCollection).resultMany();
	}
	
}
 