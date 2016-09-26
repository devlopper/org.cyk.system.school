package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public abstract class AbstractStudentClassroomSessionDivisionSubjectDetails extends AbstractOutputDetails<StudentClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText protected String classroomSessionDivisionSubject,student/*,total*//*,coefficient/*,grade,rank,outof,max,classAverage,remarks*//*,teacher*/;
	
	public AbstractStudentClassroomSessionDivisionSubjectDetails(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject) {
		super(studentClassroomSessionDivisionSubject);
		classroomSessionDivisionSubject = formatUsingBusiness(studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject());
		student = formatUsingBusiness(studentClassroomSessionDivisionSubject.getStudent());
		//coefficient = numberBusiness.format(studentSubject.getClassroomSessionDivisionSubject().getWeight());
		//teacher = studentSubject.getClassroomSessionDivisionSubject().getTeacher().getPerson().getNames();
		
	}
	
	public static final String FIELD_STUDENT = "student";
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
	
	/**/
	
	public static class SubjectDetails extends AbstractStudentClassroomSessionDivisionSubjectDetails implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		public SubjectDetails(StudentClassroomSessionDivisionSubject studentSubject) {
			super(studentSubject);
		}
	}
}
	
	