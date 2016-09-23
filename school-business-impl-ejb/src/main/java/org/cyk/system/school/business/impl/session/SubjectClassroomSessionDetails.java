package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class SubjectClassroomSessionDetails extends AbstractOutputDetails<SubjectClassroomSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String classroomSession,subject,teacher;
	
	public SubjectClassroomSessionDetails(SubjectClassroomSession subjectClassroomSession) {
		super(subjectClassroomSession);
		classroomSession = formatUsingBusiness(subjectClassroomSession.getClassroomSession());
		subject = formatUsingBusiness(subjectClassroomSession.getSubject());
		if(subjectClassroomSession.getTeacher()!=null)
			teacher = formatUsingBusiness(subjectClassroomSession.getTeacher());
	}
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_TEACHER = "teacher";
}