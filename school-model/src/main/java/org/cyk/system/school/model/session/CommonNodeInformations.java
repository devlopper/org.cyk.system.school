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
	
	//TODO do ReportTemplateIdentifiable concept
	@ManyToOne @JoinColumn(name="resultsReportTemplate") private ReportTemplate studentClassroomSessionDivisionResultsReportTemplate;
	
	@ManyToOne @JoinColumn(name="resultsReportSigner") private Person studentClassroomSessionDivisionResultsReportSigner;
	
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
			,ReportTemplate studentClassroomSessionDivisionResultsReportTemplate/*,ReportTemplate studentClassroomSessionDivisionProvisionalResultsReportTemplate*/
			,TimeDivisionType attendanceTimeDivisionType,BigDecimal evaluationPassAverage) {
		super();
		this.studentClassroomSessionDivisionResultsReportTemplate = studentClassroomSessionDivisionResultsReportTemplate;
		//this.studentClassroomSessionDivisionProvisionalResultsReportTemplate = studentClassroomSessionDivisionProvisionalResultsReportTemplate;
		this.studentSubjectAverageScale = intervalCollection;
		this.studentClassroomSessionDivisionAverageScale = intervalCollection;
		this.studentClassroomSessionAverageScale = intervalCollection;
		this.studentClassroomSessionAveragePromotionScale = studentClassroomSessionAveragePromotionScale;
		this.attendanceTimeDivisionType = attendanceTimeDivisionType;
		this.evaluationPassAverage = evaluationPassAverage;
	}
	
	public void set(CommonNodeInformations commonNodeInformations){
		this.studentClassroomSessionDivisionResultsReportTemplate = commonNodeInformations.studentClassroomSessionDivisionResultsReportTemplate;
		this.studentClassroomSessionDivisionResultsReportSigner = commonNodeInformations.studentClassroomSessionDivisionResultsReportSigner;
		this.studentSubjectAverageScale = commonNodeInformations.studentSubjectAverageScale;
		this.studentClassroomSessionDivisionAverageScale = commonNodeInformations.studentClassroomSessionDivisionAverageScale;
		this.studentClassroomSessionAverageScale = commonNodeInformations.studentClassroomSessionAverageScale;
		this.studentClassroomSessionAveragePromotionScale = commonNodeInformations.studentClassroomSessionAveragePromotionScale;
		this.attendanceTimeDivisionType = commonNodeInformations.attendanceTimeDivisionType;
		this.aggregateAttendance = commonNodeInformations.aggregateAttendance;
		this.evaluationPassAverage = commonNodeInformations.evaluationPassAverage;
		this.classroomSessionTimeDivisionType = commonNodeInformations.classroomSessionTimeDivisionType;
		this.classroomSessionDivisionOrderNumberInterval = commonNodeInformations.classroomSessionDivisionOrderNumberInterval;
		this.currentClassroomSessionDivisionIndex = commonNodeInformations.currentClassroomSessionDivisionIndex;
	}
	
	@Override
	public String getUiString() {
		return toString();
	}	
	
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_TEMPLATE = "studentClassroomSessionDivisionResultsReportTemplate";
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_SIGNER = "studentClassroomSessionDivisionResultsReportSigner";
	public static final String FIELD_STUDENT_SUBJECT_AVERAGESCALE = "studentSubjectAverageScale";
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_AVERAGE_SCALE = "studentClassroomSessionDivisionAverageScale";
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_AVERAGE_SCALE = "studentClassroomSessionAverageScale";
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_AVERAGE_PROMOTION_SCALE = "studentClassroomSessionAveragePromotionScale";
	public static final String FIELD_ATTENDANCE_TIME_DIVISION_TYPE = "attendanceTimeDivisionType";
	public static final String FIELD_AGGREGATE_ATTENDANCE = "aggregateAttendance";
	public static final String FIELD_EVALUATION_PASS_AVERAGE = "evaluationPassAverage";
	public static final String FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE = "classroomSessionTimeDivisionType";
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_ORDER_NUMBER_INTERVAL = "classroomSessionDivisionOrderNumberInterval";
	public static final String FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX = "currentClassroomSessionDivisionIndex";
}
