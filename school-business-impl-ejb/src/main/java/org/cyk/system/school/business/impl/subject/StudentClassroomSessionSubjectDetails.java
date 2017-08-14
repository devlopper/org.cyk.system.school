package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentClassroomSessionSubjectDetails extends AbstractOutputDetails<StudentClassroomSessionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	
	@Input @InputText protected FieldValue classroomSessionSubject,student/*,total*//*,coefficient/*,grade,rank,outof,max,classAverage,remarks*//*,teacher*/;
	
	public StudentClassroomSessionSubjectDetails(StudentClassroomSessionSubject studentClassroomSessionSubject) {
		super(studentClassroomSessionSubject);
	}
	
	@Override
	public void setMaster(StudentClassroomSessionSubject studentClassroomSessionSubject) {
		super.setMaster(studentClassroomSessionSubject);
		if(studentClassroomSessionSubject!=null){
			classroomSessionSubject = new FieldValue(studentClassroomSessionSubject.getClassroomSessionSubject());
			student = new FieldValue(studentClassroomSessionSubject.getStudent());	
		}
	}
	
	public static final String FIELD_STUDENT = "student";
	public static final String FIELD_CLASSROOM_SESSION_SUBJECT = "classroomSessionSubject";
}