package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionDetails;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionSubjectDetails extends AbstractOutputDetails<ClassroomSessionDivisionSubject> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @IncludeInputs private ClassroomSessionDivisionDetails classroomSessionDivisionDetails;
	@Input @InputText private String classroomSession,classroomSessionDivision,subject,coefficient,teacher;
	
	public ClassroomSessionDivisionSubjectDetails(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super(classroomSessionDivisionSubject);
		subject = formatUsingBusiness(classroomSessionDivisionSubject.getSubject());
		classroomSessionDivisionDetails = new ClassroomSessionDivisionDetails(classroomSessionDivisionSubject.getClassroomSessionDivision());
		classroomSessionDivision = formatUsingBusiness(classroomSessionDivisionSubject.getClassroomSessionDivision());
		classroomSession = formatUsingBusiness(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession());
		coefficient = formatNumber(classroomSessionDivisionSubject.getCoefficient());
		if(classroomSessionDivisionSubject.getTeacher()!=null)
			teacher = formatUsingBusiness(classroomSessionDivisionSubject.getTeacher());
	}
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_DETAILS = "classroomSessionDivisionDetails";
	public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	public static final String FIELD_COEFFICIENT = "coefficient";
	public static final String FIELD_TEACHER = "teacher";
	public static final String FIELD_SUBJECT = "subject";
}