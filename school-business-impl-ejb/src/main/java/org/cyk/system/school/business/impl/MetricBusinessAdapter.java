package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import org.cyk.system.root.business.impl.mathematics.MetricBusinessImpl;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.persistence.api.StudentResultsDao;
import org.cyk.system.school.persistence.api.StudentResultsMetricValueDao;

public class MetricBusinessAdapter extends MetricBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning implements Serializable {

	private static final long serialVersionUID = 1L;

	@Override
	public void afterCreate(Metric metric) {
		super.afterCreate(metric);
		Collection<StudentResultsMetricValue> studentResultsMetricValues = new ArrayList<>();
		for(StudentResults studentResults : inject(StudentResultsDao.class).readByMetricCollection(metric.getCollection()))
			studentResultsMetricValues.add(new StudentResultsMetricValue(studentResults, new MetricValue(metric)));
		inject(StudentResultsMetricValueBusiness.class).create(studentResultsMetricValues);
	}
	
	@Override
	public void beforeDelete(Metric metric) {
		super.beforeDelete(metric);
		Collection<StudentResultsMetricValue> studentResultsMetricValues = inject(StudentResultsMetricValueDao.class).readByMetric(metric);
		inject(StudentResultsMetricValueBusiness.class).delete(studentResultsMetricValues);
	}
	
}
