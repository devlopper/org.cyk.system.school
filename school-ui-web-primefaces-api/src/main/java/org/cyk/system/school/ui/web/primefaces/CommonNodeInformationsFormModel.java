package org.cyk.system.school.ui.web.primefaces;

import java.io.Serializable;
import java.math.BigDecimal;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputBooleanButton;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.cdi.AbstractBean;

@Getter @Setter @Accessors(chain=true)
public class CommonNodeInformationsFormModel extends AbstractBean implements Serializable {

	private static final long serialVersionUID = -465747050467060317L;
 
	@Input @InputChoice @InputOneChoice @InputOneCombo private ReportTemplate studentClassroomSessionDivisionResultsReportTemplate;
	@Input @InputChoice @InputOneChoice @InputOneCombo private Person studentClassroomSessionDivisionResultsReportSigner;
	
	@Input @InputChoice @InputOneChoice @InputOneCombo private IntervalCollection studentSubjectAverageScale;
	@Input @InputChoice @InputOneChoice @InputOneCombo private IntervalCollection studentClassroomSessionDivisionAverageScale;
	@Input @InputChoice @InputOneChoice @InputOneCombo private IntervalCollection studentClassroomSessionAverageScale;
	@Input @InputChoice @InputOneChoice @InputOneCombo private IntervalCollection studentClassroomSessionAveragePromotionScale;
	@Input @InputChoice @InputOneChoice @InputOneCombo private TimeDivisionType attendanceTimeDivisionType;
	
	@Input @InputBooleanButton private Boolean aggregateAttendance;
	@Input @InputNumber private BigDecimal evaluationPassAverage;
	
	@Input @InputChoice @InputOneChoice @InputOneCombo private TimeDivisionType classroomSessionTimeDivisionType;
	@Input @InputChoice @InputOneChoice @InputOneCombo private Interval classroomSessionDivisionOrderNumberInterval;
	
	@Input @InputNumber private Long currentClassroomSessionDivisionIndex;
	
	public void set(CommonNodeInformations commonNodeInformations){
		if(commonNodeInformations==null)
			return;
		studentClassroomSessionDivisionResultsReportTemplate = commonNodeInformations.getStudentClassroomSessionDivisionResultsReportTemplate();
		studentClassroomSessionDivisionResultsReportSigner = commonNodeInformations.getStudentClassroomSessionDivisionResultsReportSigner();
		studentSubjectAverageScale = commonNodeInformations.getStudentSubjectAverageScale();
		studentClassroomSessionDivisionAverageScale = commonNodeInformations.getStudentClassroomSessionDivisionAverageScale();
		studentClassroomSessionAverageScale = commonNodeInformations.getStudentClassroomSessionAverageScale();
		studentClassroomSessionAveragePromotionScale = commonNodeInformations.getStudentClassroomSessionAveragePromotionScale();
		attendanceTimeDivisionType = commonNodeInformations.getAttendanceTimeDivisionType();
		aggregateAttendance = commonNodeInformations.getAggregateAttendance();
		evaluationPassAverage = commonNodeInformations.getEvaluationPassAverage();
		classroomSessionTimeDivisionType = commonNodeInformations.getClassroomSessionTimeDivisionType();
		classroomSessionDivisionOrderNumberInterval = commonNodeInformations.getClassroomSessionDivisionOrderNumberInterval();
		currentClassroomSessionDivisionIndex = commonNodeInformations.getCurrentClassroomSessionDivisionIndex();
	}
	
	public void write(CommonNodeInformations commonNodeInformations) {
		commonNodeInformations.setStudentClassroomSessionDivisionResultsReportTemplate(studentClassroomSessionDivisionResultsReportTemplate);
		commonNodeInformations.setStudentClassroomSessionDivisionResultsReportSigner(studentClassroomSessionDivisionResultsReportSigner);
		commonNodeInformations.setStudentSubjectAverageScale(studentSubjectAverageScale);
		commonNodeInformations.setStudentClassroomSessionDivisionAverageScale(studentClassroomSessionDivisionAverageScale);
		commonNodeInformations.setStudentClassroomSessionAverageScale(studentClassroomSessionAverageScale);
		commonNodeInformations.setStudentClassroomSessionAveragePromotionScale(studentClassroomSessionAveragePromotionScale);
		commonNodeInformations.setAttendanceTimeDivisionType(attendanceTimeDivisionType);
		commonNodeInformations.setAggregateAttendance(aggregateAttendance);
		commonNodeInformations.setEvaluationPassAverage(evaluationPassAverage);
		commonNodeInformations.setClassroomSessionTimeDivisionType(classroomSessionTimeDivisionType);
		commonNodeInformations.setClassroomSessionDivisionOrderNumberInterval(classroomSessionDivisionOrderNumberInterval);
		commonNodeInformations.setCurrentClassroomSessionDivisionIndex(currentClassroomSessionDivisionIndex);
	}
	
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_TEMPLATE = "studentClassroomSessionDivisionResultsReportTemplate";
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_SIGNER = "studentClassroomSessionDivisionResultsReportSigner";
	public static final String FIELD_STUDENT_SUBJECT_AVERAGE_SCALE = "studentSubjectAverageScale";
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
