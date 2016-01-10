package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentClassroomSessionDetails extends AbstractOutputDetails<StudentClassroomSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String registrationCode;
	@Input @InputText private String names;
	
	public StudentClassroomSessionDetails(StudentClassroomSession studentClassroomSession) {
		super(studentClassroomSession);
		registrationCode = studentClassroomSession.getStudent().getRegistration().getCode();
		names = studentClassroomSession.getStudent().getPerson().getNames();
	}
}