package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractModelElementOutputDetails;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class CommonNodeInformationsDetails extends AbstractModelElementOutputDetails<CommonNodeInformations> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private FieldValue studentClassroomSessionDivisionResultsReportTemplate,studentClassroomSessionDivisionResultsReportSigner
		,studentSubjectAverageScale,studentClassroomSessionDivisionAverageScale,studentClassroomSessionAverageScale,studentClassroomSessionAveragePromotionScale
		,attendanceTimeDivisionType,classroomSessionTimeDivisionType,classroomSessionDivisionOrderNumberInterval;
	@Input @InputText private String aggregateAttendance,evaluationPassAverage,currentClassroomSessionDivisionIndex;
		
	public CommonNodeInformationsDetails(CommonNodeInformations commonNodeInformations) {
		super(commonNodeInformations);
	}
	
	@Override
	public void setMaster(CommonNodeInformations commonNodeInformations) {
		super.setMaster(commonNodeInformations);
		if(commonNodeInformations!=null){
			aggregateAttendance = formatResponse(commonNodeInformations.getAggregateAttendance());
			currentClassroomSessionDivisionIndex = formatNumber(commonNodeInformations.getCurrentClassroomSessionDivisionIndex());
			evaluationPassAverage = formatNumber(commonNodeInformations.getEvaluationPassAverage());
			
			if(commonNodeInformations.getStudentClassroomSessionDivisionResultsReportTemplate()!=null)
				studentClassroomSessionDivisionResultsReportTemplate = new FieldValue(commonNodeInformations.getStudentClassroomSessionDivisionResultsReportTemplate());
			if(commonNodeInformations.getStudentClassroomSessionDivisionResultsReportSigner()!=null)
				studentClassroomSessionDivisionResultsReportSigner = new FieldValue(commonNodeInformations.getStudentClassroomSessionDivisionResultsReportSigner());
			if(commonNodeInformations.getStudentSubjectAverageScale()!=null)
				studentSubjectAverageScale = new FieldValue(commonNodeInformations.getStudentSubjectAverageScale());
			if(commonNodeInformations.getStudentClassroomSessionDivisionAverageScale()!=null)
				studentClassroomSessionDivisionAverageScale = new FieldValue(commonNodeInformations.getStudentClassroomSessionDivisionAverageScale());
			if(commonNodeInformations.getStudentClassroomSessionAverageScale()!=null)
				studentClassroomSessionAverageScale = new FieldValue(commonNodeInformations.getStudentClassroomSessionAverageScale());
			if(commonNodeInformations.getStudentClassroomSessionAveragePromotionScale()!=null)
				studentClassroomSessionAveragePromotionScale = new FieldValue(commonNodeInformations.getStudentClassroomSessionAveragePromotionScale());
			if(commonNodeInformations.getAttendanceTimeDivisionType()!=null)
				attendanceTimeDivisionType = new FieldValue(commonNodeInformations.getAttendanceTimeDivisionType());
			if(commonNodeInformations.getClassroomSessionTimeDivisionType()!=null)
				classroomSessionTimeDivisionType = new FieldValue(commonNodeInformations.getClassroomSessionTimeDivisionType());
			if(commonNodeInformations.getClassroomSessionDivisionOrderNumberInterval()!=null)
				classroomSessionDivisionOrderNumberInterval = new FieldValue(commonNodeInformations.getClassroomSessionDivisionOrderNumberInterval());
				
		}
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