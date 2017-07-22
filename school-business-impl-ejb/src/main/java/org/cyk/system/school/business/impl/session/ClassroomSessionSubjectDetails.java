package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionSubjectDetails extends AbstractOutputDetails<ClassroomSessionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String classroomSession,subject,teacher;
	
	public ClassroomSessionSubjectDetails(ClassroomSessionSubject classroomSessionSubject) {
		super(classroomSessionSubject);
		classroomSession = formatUsingBusiness(classroomSessionSubject.getClassroomSession());
		subject = formatUsingBusiness(classroomSessionSubject.getSubject());
		if(classroomSessionSubject.getTeacher()!=null)
			teacher = formatUsingBusiness(classroomSessionSubject.getTeacher());
	}
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_TEACHER = "teacher";
}