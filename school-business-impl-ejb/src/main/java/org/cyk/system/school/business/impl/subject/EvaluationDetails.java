package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.subject.Evaluation;

public class EvaluationDetails extends AbstractOutputDetails<Evaluation> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	public EvaluationDetails(Evaluation subjectEvaluation) {
		super(subjectEvaluation);
		name = subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getName();
		weight = numberBusiness.format(subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getWeight());
	}
}