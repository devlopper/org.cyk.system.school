package org.cyk.system.school.model;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.MetricValue;

@Getter @Setter @Entity @NoArgsConstructor @Deprecated
public class StudentResultsMetricValue extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private StudentResults studentResults;
	
	@OneToOne(cascade={CascadeType.ALL}) @NotNull private MetricValue metricValue;
	
	public StudentResultsMetricValue(StudentResults studentResults,MetricValue metricValue) {
		super();
		this.studentResults = studentResults;
		this.metricValue = metricValue;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this,ToStringStyle.SHORT_PREFIX_STYLE);
	}
	
	/**/
	
	public static final String FIELD_STUDENT_RESULTS = "studentResults";
	public static final String FIELD_METRIC_VALUE = "metricValue";

}
