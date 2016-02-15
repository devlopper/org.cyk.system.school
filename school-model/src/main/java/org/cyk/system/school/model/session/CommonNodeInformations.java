package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractModelElement;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.time.TimeDivisionType;

@Embeddable @Getter @Setter @NoArgsConstructor
public class CommonNodeInformations extends AbstractModelElement implements Serializable {

	private static final long serialVersionUID = 3372342222993865767L;
	
	@ManyToOne @JoinColumn(name="resultsReportTemplate") private ReportTemplate studentClassroomSessionDivisionResultsReportTemplate;
	@ManyToOne private IntervalCollection studentSubjectAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionDivisionAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionAverageScale;
	
	@ManyToOne private MetricCollection studentWorkMetricCollection;
	@ManyToOne private TimeDivisionType attendanceTimeDivisionType;
	
	@Column private Boolean aggregateAttendance;
	
	@ManyToOne private TimeDivisionType classroomSessionTimeDivisionType;
	private Byte currentClassroomSessionDivisionIndex = 0;
	
	/*
	@ManyToOne private FiniteStateMachineState finiteStateMachineState;
	@ManyToOne private FiniteStateMachineState finiteStateMachineState;
	*/
	public CommonNodeInformations(IntervalCollection intervalCollection,MetricCollection studentWorkMetricCollection,ReportTemplate studentClassroomSessionDivisionResultsReportTemplate
			,TimeDivisionType attendanceTimeDivisionType) {
		super();
		this.studentClassroomSessionDivisionResultsReportTemplate = studentClassroomSessionDivisionResultsReportTemplate;
		this.studentSubjectAverageScale = intervalCollection;
		this.studentClassroomSessionDivisionAverageScale = intervalCollection;
		this.studentClassroomSessionAverageScale = intervalCollection;
		this.studentWorkMetricCollection = studentWorkMetricCollection;
		this.attendanceTimeDivisionType = attendanceTimeDivisionType;
	}
	
	@Override
	public String getUiString() {
		return toString();
	}	
	
}
