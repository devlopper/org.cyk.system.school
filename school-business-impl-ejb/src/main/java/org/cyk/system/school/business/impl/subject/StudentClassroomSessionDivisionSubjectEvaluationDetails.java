package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class StudentClassroomSessionDivisionSubjectEvaluationDetails extends AbstractOutputDetails<StudentClassroomSessionDivisionSubjectEvaluation> implements Serializable{
	
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String names,mark;
	
	public StudentClassroomSessionDivisionSubjectEvaluationDetails(StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation) {
		super(studentClassroomSessionDivisionSubjectEvaluation);
		names = studentClassroomSessionDivisionSubjectEvaluation.getStudentSubject().getStudent().getCode()+Constant.CHARACTER_SLASH
				+studentClassroomSessionDivisionSubjectEvaluation.getStudentSubject().getStudent().getPerson().getNames();
		mark = formatNumber(studentClassroomSessionDivisionSubjectEvaluation.getValue());
	}
	
	public static final String FIELD_NAMES = "names";
	public static final String FIELD_MARK = "mark";
	
}