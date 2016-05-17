package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentClassroomSessionDivisionDetails extends AbstractOutputDetails<StudentClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String registrationCode,names,evaluationAverage,numberOfTimeAbsent,globalAppreciation,conferenceRequested;
	
	public StudentClassroomSessionDivisionDetails(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super(studentClassroomSessionDivision);
		registrationCode = studentClassroomSessionDivision.getStudent().getRegistration().getCode();
		names = studentClassroomSessionDivision.getStudent().getPerson().getNames();
		globalAppreciation = studentClassroomSessionDivision.getResults().getAppreciation();
		conferenceRequested = formatResponse(studentClassroomSessionDivision.getResults().getConferenceRequested());
		
		if(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue()!=null)
			evaluationAverage = numberBusiness.format(studentClassroomSessionDivision.getResults().getEvaluationSort().getAverage().getValue());
		
		if(studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration()!=null)
			numberOfTimeAbsent = numberBusiness.format(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().convertAttendanceTimeToDivisionDuration(
					studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession(),studentClassroomSessionDivision.getResults().getLectureAttendance().getMissedDuration()));
	}
}