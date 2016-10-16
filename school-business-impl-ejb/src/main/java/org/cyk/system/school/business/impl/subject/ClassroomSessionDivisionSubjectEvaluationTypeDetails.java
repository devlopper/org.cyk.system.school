package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionSubjectEvaluationTypeDetails extends AbstractOutputDetails<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String classroomSessionDivisionSubject,coefficient,maximum;
	
	public ClassroomSessionDivisionSubjectEvaluationTypeDetails(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		super(classroomSessionDivisionSubjectEvaluationType);
		classroomSessionDivisionSubject = formatUsingBusiness(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject());
		maximum = formatNumber(classroomSessionDivisionSubjectEvaluationType.getWeight());
	}
	
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
	public static final String FIELD_MAXIMUM = "maximum";
	
}