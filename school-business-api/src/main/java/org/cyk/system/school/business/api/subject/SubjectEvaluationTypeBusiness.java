package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface SubjectEvaluationTypeBusiness extends TypedBusiness<SubjectEvaluationType> {

	SubjectEvaluationType findBySubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName);

	Collection<SubjectEvaluationType> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
}
