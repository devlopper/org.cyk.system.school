package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import org.cyk.system.root.model.AbstractModelElement;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.time.TimeDivisionType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Embeddable @Getter @Setter @NoArgsConstructor
public class CommonNodeInformations extends AbstractModelElement implements Serializable {

	private static final long serialVersionUID = 3372342222993865767L;
	
	@ManyToOne @JoinColumn(name="resultsReportTemplate") private ReportTemplate studentClassroomSessionDivisionResultsReportTemplate;
	@ManyToOne private Person studentClassroomSessionDivisionResultsReportSigner;
	
	@ManyToOne private IntervalCollection studentSubjectAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionDivisionAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionAveragePromotionScale;
	
	@ManyToOne private TimeDivisionType attendanceTimeDivisionType;
	
	@Column private Boolean aggregateAttendance;
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal evaluationPassAverage;
	
	@ManyToOne private TimeDivisionType classroomSessionTimeDivisionType;
	
	@ManyToOne private Interval classroomSessionDivisionOrderNumberInterval;
	
	private Long currentClassroomSessionDivisionIndex;
	
	public CommonNodeInformations(IntervalCollection intervalCollection,IntervalCollection studentClassroomSessionAveragePromotionScale
			,ReportTemplate studentClassroomSessionDivisionResultsReportTemplate,TimeDivisionType attendanceTimeDivisionType,BigDecimal evaluationPassAverage) {
		super();
		this.studentClassroomSessionDivisionResultsReportTemplate = studentClassroomSessionDivisionResultsReportTemplate;
		this.studentSubjectAverageScale = intervalCollection;
		this.studentClassroomSessionDivisionAverageScale = intervalCollection;
		this.studentClassroomSessionAverageScale = intervalCollection;
		this.studentClassroomSessionAveragePromotionScale = studentClassroomSessionAveragePromotionScale;
		this.attendanceTimeDivisionType = attendanceTimeDivisionType;
		this.evaluationPassAverage = evaluationPassAverage;
	}
	
	@Override
	public String getUiString() {
		return toString();
	}	
	
}
