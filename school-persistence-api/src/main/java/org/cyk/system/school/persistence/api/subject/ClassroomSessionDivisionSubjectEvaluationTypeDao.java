package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface ClassroomSessionDivisionSubjectEvaluationTypeDao extends TypedDao<ClassroomSessionDivisionSubjectEvaluationType> {

	ClassroomSessionDivisionSubjectEvaluationType readBySubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName);

	Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
}
