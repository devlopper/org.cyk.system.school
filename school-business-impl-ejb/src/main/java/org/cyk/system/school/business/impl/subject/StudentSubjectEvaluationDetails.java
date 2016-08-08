package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentSubjectEvaluationDetails extends AbstractOutputDetails<StudentClassroomSessionDivisionSubjectEvaluation> implements Serializable{
	
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String registrationCode,names,mark;
	
	public StudentSubjectEvaluationDetails(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation) {
		super(studentSubjectEvaluation);
		registrationCode = studentSubjectEvaluation.getStudentSubject().getStudent().getCode();
		names = studentSubjectEvaluation.getStudentSubject().getStudent().getPerson().getNames();
		mark = numberBusiness.format(studentSubjectEvaluation.getValue());
	}
	
}