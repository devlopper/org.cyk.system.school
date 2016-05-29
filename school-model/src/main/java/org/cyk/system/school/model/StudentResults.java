package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.AssociationOverride;
import javax.persistence.AssociationOverrides;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
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
import org.cyk.utility.common.Constant;

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
	@AssociationOverrides(value={
			@AssociationOverride(name= Sort.FIELD_AVERAGE_APPRECIATED_INTERVAL,joinColumns = @JoinColumn(name="evaluationAppreciatedAverageInterval"))
			,@AssociationOverride(name= Sort.FIELD_AVERAGE_PROMOTED_INTERVAL,joinColumns = @JoinColumn(name="evaluationPromotedAverageInterval"))
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
	@AssociationOverrides(value={
			@AssociationOverride(name= Sort.FIELD_AVERAGE_APPRECIATED_INTERVAL,joinColumns = @JoinColumn(name="lectureAppreciatedAttendanceAverageInterval"))
			,@AssociationOverride(name= Sort.FIELD_AVERAGE_PROMOTED_INTERVAL,joinColumns = @JoinColumn(name="lecturePromotedAttendanceAverageInterval"))
			})
	private Sort lectureAttendanceSort = new Sort();
		
	@Embedded private Attendance lectureAttendance = new Attendance();
	
	@Column(length= 1024 * 1)
	private String appreciation;
	
	@OneToOne private File report;
	
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
		return String.format(LOG_FORMAT, evaluationSort==null ?Constant.EMPTY_STRING:evaluationSort.getLogMessage()
				,lectureAttendance==null ?Constant.EMPTY_STRING:lectureAttendance.getLogMessage()
						,lectureAttendanceSort==null ?Constant.EMPTY_STRING:lectureAttendanceSort.getLogMessage(),appreciation,conferenceRequested);
	}
	private static final String LOG_FORMAT = StudentResults.class.getSimpleName()+"(%s %s %s APP=%s CR=%s)";

	public static final String FIELD_EVALUATION_SORT = "evaluationSort";
}
