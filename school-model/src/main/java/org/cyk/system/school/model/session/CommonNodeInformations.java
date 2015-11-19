package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractModelElement;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.time.TimeDivisionType;

@Embeddable @Getter @Setter @NoArgsConstructor
public class CommonNodeInformations extends AbstractModelElement implements Serializable {

	private static final long serialVersionUID = 3372342222993865767L;
	
	@ManyToOne @JoinColumn(name="resultsReportFile") private File studentClassroomSessionDivisionResultsReportFile;
	@ManyToOne private IntervalCollection studentSubjectAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionDivisionAverageScale;
	@ManyToOne private IntervalCollection studentClassroomSessionAverageScale;
	
	@ManyToOne private MetricCollection studentWorkMetricCollection;
	@ManyToOne private TimeDivisionType attendanceTimeDivisionType;
	
	private Boolean aggregateAttendance = Boolean.TRUE;
	
	public CommonNodeInformations(IntervalCollection intervalCollection,MetricCollection studentWorkMetricCollection,File studentClassroomSessionDivisionResultsReportFile,
			TimeDivisionType attendanceTimeDivisionType) {
		super();
		this.studentClassroomSessionDivisionResultsReportFile = studentClassroomSessionDivisionResultsReportFile;
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
