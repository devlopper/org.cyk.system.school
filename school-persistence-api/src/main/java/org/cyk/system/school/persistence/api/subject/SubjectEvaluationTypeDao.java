package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface SubjectEvaluationTypeDao extends TypedDao<SubjectEvaluationType> {

	SubjectEvaluationType readBySubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName);

	Collection<SubjectEvaluationType> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
}
