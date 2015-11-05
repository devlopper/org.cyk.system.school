package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;

import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentResultsMetricValueDetails extends AbstractOutputDetails<StudentResultsMetricValue> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	@Input @InputText private String name,value;
	public StudentResultsMetricValueDetails(StudentResultsMetricValue studentResultsMetricValue) {
		super(studentResultsMetricValue);
		name = studentResultsMetricValue.getMetricValue().getMetric().getName();
		value = numberBusiness.format(studentResultsMetricValue.getMetricValue().getValue());
	}
}