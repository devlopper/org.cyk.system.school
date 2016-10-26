package org.cyk.system.school.persistence.api;

import java.util.Collection;

import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;

public interface StudentResultsMetricValueDao extends TypedDao<StudentResultsMetricValue> {

	Collection<StudentResultsMetricValue> readByStudentResults(StudentResults studentResults);

	Collection<StudentResultsMetricValue> readByStudentResultsByMetricCollection(StudentResults studentResults,MetricCollection metricCollection);

	Collection<StudentResultsMetricValue> readByMetric(Metric metric);
	
}
