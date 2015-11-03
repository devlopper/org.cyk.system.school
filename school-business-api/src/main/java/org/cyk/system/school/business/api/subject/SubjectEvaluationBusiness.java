package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;

public interface SubjectEvaluationBusiness extends TypedBusiness<SubjectEvaluation> {

	SubjectEvaluation newInstance(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
	SubjectEvaluation save(SubjectEvaluation subjectEvaluation,Collection<StudentSubjectEvaluation> studentSubjectEvaluations);
}
