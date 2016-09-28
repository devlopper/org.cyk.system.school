package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Getter @Setter
public class EvaluationDetails extends AbstractOutputDetails<Evaluation> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String classroomSession,classroomSessionDivision,classroomSessionDivisionSubject
		,classroomSessionDivisionSubjectEvaluationType;
	
	public EvaluationDetails(Evaluation evaluation) {
		super(evaluation);
		name = evaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getName();
		weight = numberBusiness.format(evaluation.getClassroomSessionDivisionSubjectEvaluationType().getWeight());
		classroomSessionDivisionSubjectEvaluationType = formatUsingBusiness(evaluation.getClassroomSessionDivisionSubjectEvaluationType()); 
		classroomSessionDivisionSubject = formatUsingBusiness(evaluation.getClassroomSessionDivisionSubjectEvaluationType()
				.getClassroomSessionDivisionSubject()); 
		classroomSessionDivision = formatUsingBusiness(evaluation.getClassroomSessionDivisionSubjectEvaluationType()
				.getClassroomSessionDivisionSubject().getClassroomSessionDivision()); 
		classroomSession = formatUsingBusiness(evaluation.getClassroomSessionDivisionSubjectEvaluationType()
				.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()); 
	}
	
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_TYPE = "classroomSessionDivisionSubjectEvaluationType";
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
	public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
}