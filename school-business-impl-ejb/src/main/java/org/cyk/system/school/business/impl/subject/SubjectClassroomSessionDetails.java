package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class SubjectClassroomSessionDetails extends AbstractOutputDetails<SubjectClassroomSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String name;
	
	public SubjectClassroomSessionDetails(SubjectClassroomSession subjectClassroomSession) {
		super(subjectClassroomSession);
		name = subjectClassroomSession.getSubject().getName();
	}
}