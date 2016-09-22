package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public abstract class AbstractSubjectDetails extends AbstractOutputDetails<StudentClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	@Input @InputText protected String subject/*,total*//*,coefficient/*,grade,rank,outof,max,classAverage,remarks*//*,teacher*/;
	public AbstractSubjectDetails(StudentClassroomSessionDivisionSubject studentSubject) {
		super(studentSubject);
		subject = studentSubject.getClassroomSessionDivisionSubject().getSubject().getName();
		//coefficient = numberBusiness.format(studentSubject.getClassroomSessionDivisionSubject().getWeight());
		//teacher = studentSubject.getClassroomSessionDivisionSubject().getTeacher().getPerson().getNames();
		
	}
	
	public static final String FILED_SUBJECT = "subject";
	
	/**/
	
	public static class SubjectDetails extends AbstractSubjectDetails implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		public SubjectDetails(StudentClassroomSessionDivisionSubject studentSubject) {
			super(studentSubject);
		}
	}
}
	
	