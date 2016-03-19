package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentSubject;

public class StudentSubjectDetails extends AbstractOutputDetails<StudentSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	
	public StudentSubjectDetails(StudentSubject studentSubject) {
		super(studentSubject);
		
	}
}