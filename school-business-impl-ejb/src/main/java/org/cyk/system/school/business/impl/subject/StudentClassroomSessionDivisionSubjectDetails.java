package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;

public class StudentClassroomSessionDivisionSubjectDetails extends AbstractOutputDetails<StudentClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	
	public StudentClassroomSessionDivisionSubjectDetails(StudentClassroomSessionDivisionSubject studentSubject) {
		super(studentSubject);
		
	}
}