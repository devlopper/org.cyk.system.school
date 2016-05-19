package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentClassroomSessionDivisionBroadsheetDetails extends AbstractOutputDetails<StudentClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String registrationCode,names,_1;
	
	public StudentClassroomSessionDivisionBroadsheetDetails(StudentClassroomSessionDivision studentClassroomSessionDivision) {
		super(studentClassroomSessionDivision);
		registrationCode = studentClassroomSessionDivision.getStudent().getRegistration().getCode();
		names = studentClassroomSessionDivision.getStudent().getPerson().getNames();
		
	}
}