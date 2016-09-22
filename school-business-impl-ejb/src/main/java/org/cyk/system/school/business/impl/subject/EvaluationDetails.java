package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class EvaluationDetails extends AbstractOutputDetails<Evaluation> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String name,coefficient;
	
	public EvaluationDetails(Evaluation subjectEvaluation) {
		super(subjectEvaluation);
		name = subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getName();
		coefficient = numberBusiness.format(subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getWeight());
	}
}