package org.cyk.system.school.model;

import java.io.Serializable;

import javax.persistence.AssociationOverride;
import javax.persistence.AssociationOverrides;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.Sort;
import org.cyk.system.root.model.search.AbstractFieldValueSearchCriteriaSet;
import org.cyk.system.root.model.time.Attendance;
import org.cyk.utility.common.Constant;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentResults extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@Embedded @AttributeOverrides(value={
			@AttributeOverride(name="average.dividend",column=@Column(name="evaluationAverageDividend",precision=20,scale=FLOAT_SCALE))
			,@AttributeOverride(name="average.divisor",column=@Column(name="evaluationAverageDivisor",precision=20,scale=FLOAT_SCALE))
			,@AttributeOverride(name="average.value",column=@Column(name="evaluationAverageValue",precision=20,scale=FLOAT_SCALE))
			
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
			@AttributeOverride(name="average.dividend",column=@Column(name="lectureAttendanceAverageDividend",precision=20,scale=FLOAT_SCALE))
			,@AttributeOverride(name="average.divisor",column=@Column(name="lectureAttendanceAverageDivisor",precision=20,scale=FLOAT_SCALE))
			,@AttributeOverride(name="average.value",column=@Column(name="lectureAttendanceAverageValue",precision=20,scale=FLOAT_SCALE))
			
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
	
	private Boolean conferenceRequested;
	
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
	
	/**/
	
	@Getter @Setter
	public static class SearchCriteria extends AbstractFieldValueSearchCriteriaSet implements Serializable {

		private static final long serialVersionUID = 6796076474234170332L;

		private Sort.SearchCriteria evaluationSort = new Sort.SearchCriteria();
		
	}
}
