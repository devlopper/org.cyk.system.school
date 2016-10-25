package org.cyk.system.school.persistence.api;

import java.util.Collection;

import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.StudentResults;

public interface StudentResultsDao extends TypedDao<StudentResults> {

	Collection<StudentResults> readByMetricCollection(MetricCollection metricCollection);
	
}
