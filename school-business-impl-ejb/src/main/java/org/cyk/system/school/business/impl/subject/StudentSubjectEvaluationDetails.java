package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentSubjectEvaluationDetails extends AbstractOutputDetails<StudentSubjectEvaluation> implements Serializable{
	
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String registrationNumber,student,mark;
	
	public StudentSubjectEvaluationDetails(StudentSubjectEvaluation studentSubjectEvaluation) {
		super(studentSubjectEvaluation);
		registrationNumber = studentSubjectEvaluation.getStudentSubject().getStudent().getRegistration().getCode();
		student = studentSubjectEvaluation.getStudentSubject().getStudent().getPerson().getNames();
		mark = numberBusiness.format(studentSubjectEvaluation.getValue());
	}
	
}