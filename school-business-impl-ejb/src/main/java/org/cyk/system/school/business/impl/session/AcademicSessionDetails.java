package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class AcademicSessionDetails extends AbstractOutputDetails<AcademicSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String nextStartingDate,attendanceTimeDivisionType,aggregateAttendance,classroomSessionTimeDivisionType,currentClassroomSessionDivisionIndex;
		
	public AcademicSessionDetails(AcademicSession academicSession) {
		super(academicSession);
		nextStartingDate = formatDate(academicSession.getNextStartingDate());
		attendanceTimeDivisionType = formatUsingBusiness(academicSession.getNodeInformations().getAttendanceTimeDivisionType());
		aggregateAttendance = formatResponse(academicSession.getNodeInformations().getAggregateAttendance());
		classroomSessionTimeDivisionType = formatUsingBusiness(academicSession.getNodeInformations().getClassroomSessionTimeDivisionType());
		currentClassroomSessionDivisionIndex = formatUsingBusiness(academicSession.getNodeInformations().getCurrentClassroomSessionDivisionIndex());
	}
	
	public static final String FIELD_NEXT_STARTING_DATE = "nextStartingDate";
	public static final String FIELD_ATTENDANCE_TIME_DIVISION_TYPE = "attendanceTimeDivisionType";
	public static final String FIELD_AGGREGATE_ATTENDANCE = "aggregateAttendance";
	public static final String FIELD_CLASSROOM_SESSION_TIME_DIVISION_TYPE = "classroomSessionTimeDivisionType";
	public static final String FIELD_CURRENT_CLASSROOM_SESSION_DIVISION_INDEX = "currentClassroomSessionDivisionIndex";
}