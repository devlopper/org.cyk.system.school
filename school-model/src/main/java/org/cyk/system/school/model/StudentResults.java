package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.root.model.mathematics.Sort;
import org.cyk.system.root.model.time.Attendance;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentResults extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@Embedded @AttributeOverrides(value={
			@AttributeOverride(name="average.dividend",column=@Column(name="evaluationAverageDividend"))
			,@AttributeOverride(name="average.divisor",column=@Column(name="evaluationAverageDivisor"))
			,@AttributeOverride(name="average.value",column=@Column(name="evaluationAverageValue"))
			
			,@AttributeOverride(name="rank.sequenceOrder",column=@Column(name="evaluationRankSequenceOrder"))
			,@AttributeOverride(name="rank.value",column=@Column(name="evaluationRankValue"))
			,@AttributeOverride(name="rank.exaequo",column=@Column(name="evaluationRankExaequo"))
			
			,@AttributeOverride(name="comments",column=@Column(name="evaluationComments"))
	})
	private Sort evaluationSort = new Sort();
	
	@Embedded @AttributeOverrides(value={
			@AttributeOverride(name="average.dividend",column=@Column(name="lectureAttendanceAverageDividend"))
			,@AttributeOverride(name="average.divisor",column=@Column(name="lectureAttendanceAverageDivisor"))
			,@AttributeOverride(name="average.value",column=@Column(name="lectureAttendanceAverageValue"))
			
			,@AttributeOverride(name="rank.sequenceOrder",column=@Column(name="lectureAttendanceRankSequenceOrder"))
			,@AttributeOverride(name="rank.value",column=@Column(name="lectureAttendanceRankValue"))
			,@AttributeOverride(name="rank.exaequo",column=@Column(name="lectureAttendanceRankExaequo"))
			
			,@AttributeOverride(name="comments",column=@Column(name="lectureAttendanceComments"))
	})
	private Sort lectureAttendanceSort = new Sort();
		
	@Embedded private Attendance lectureAttendance = new Attendance();
	
	private String appreciation;
	
	@OneToMany(fetch=FetchType.EAGER,cascade={CascadeType.ALL}) 
	private Collection<MetricValue> metricValues = new ArrayList<>();
	
	@OneToOne private File report;
	
	/**/
	
	@Override
	public String getUiString() {
		return toString();
	}
	
	/*
	@Override
	public String toString() {
		return evaluationSort+" , "+lectureAttendanceSort;
	}*/
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this,ToStringStyle.SHORT_PREFIX_STYLE);
	}

}
