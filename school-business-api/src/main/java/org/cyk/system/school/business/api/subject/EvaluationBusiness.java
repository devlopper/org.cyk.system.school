package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Evaluation;

public interface EvaluationBusiness extends TypedBusiness<Evaluation> {

	Evaluation newInstance(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
	Evaluation save(Evaluation evaluation,Collection<StudentSubjectEvaluation> studentSubjectEvaluations);
}
