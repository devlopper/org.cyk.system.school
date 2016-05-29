package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentClassroomSessionDivisionSubjectDetails extends AbstractOutputDetails<StudentClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String names;
	
	public StudentClassroomSessionDivisionSubjectDetails(StudentClassroomSessionDivisionSubject studentSubject) {
		super(studentSubject);
		names = studentSubject.getStudent().getPerson().getNames();
	}
}