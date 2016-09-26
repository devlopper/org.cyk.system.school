package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;

public class StudentClassroomSessionDivisionSubjectDetails extends AbstractStudentClassroomSessionDivisionSubjectDetails implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	
	public StudentClassroomSessionDivisionSubjectDetails(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject) {
		super(studentClassroomSessionDivisionSubject);
		
	}
}