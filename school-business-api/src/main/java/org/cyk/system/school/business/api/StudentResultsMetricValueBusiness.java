package org.cyk.system.school.business.api;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;

public interface StudentResultsMetricValueBusiness extends TypedBusiness<StudentResultsMetricValue> {

	Collection<StudentResultsMetricValue> findByStudentResults(StudentResults studentResults);
	Collection<StudentResultsMetricValue> findByStudentResultsByMetricCollection(StudentResults studentResults,MetricCollection metricCollection);
	
}
