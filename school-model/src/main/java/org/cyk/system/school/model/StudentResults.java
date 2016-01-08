package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
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
			
			//,@AttributeOverride(name="averageInterval",column=@Column(name="evaluationAverageInterval"))
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
			
			//,@AttributeOverride(name="averageInterval",column=@Column(name="lectureAttendanceAverageInterval"))
	})
	private Sort lectureAttendanceSort = new Sort();
		
	@Embedded private Attendance lectureAttendance = new Attendance();
	
	private String appreciation;
	
	@OneToOne private File report;
	
	private Boolean promoted;
	private Boolean conferenceRequested;
	
	@Transient private Collection<StudentResultsMetricValue> studentResultsMetricValues;
	
	/**/
	
	public Collection<StudentResultsMetricValue> getStudentResultsMetricValues(){
		if(studentResultsMetricValues==null)
			studentResultsMetricValues = new ArrayList<>();
		return studentResultsMetricValues;
	}
	
	public Sort getEvaluationSort(){
		if(evaluationSort==null)
			evaluationSort = new Sort();
		return evaluationSort;
	}
	
	public Attendance getLectureAttendance(){
		if(lectureAttendance==null)
			lectureAttendance = new Attendance();
		return lectureAttendance;
	}
	
	/**/
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this,ToStringStyle.SHORT_PREFIX_STYLE);
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, evaluationSort.getLogMessage(),lectureAttendance.getLogMessage(),lectureAttendanceSort.getLogMessage(),appreciation,promoted,conferenceRequested);
	}
	private static final String LOG_FORMAT = StudentResults.class.getSimpleName()+"(%s %s %s APP=%s PROM=%s CR=%s)";

}
