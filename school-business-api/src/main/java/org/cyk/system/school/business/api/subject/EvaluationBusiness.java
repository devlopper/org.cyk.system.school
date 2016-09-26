package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Evaluation;

public interface EvaluationBusiness extends TypedBusiness<Evaluation> {

	Evaluation instanciateOne(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
	Evaluation instanciateOne(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType);
	
	Evaluation save(Evaluation evaluation,Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations);
	
	Collection<Evaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);

	Collection<Evaluation> findByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType);
}
