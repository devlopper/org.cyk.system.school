package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.SubjectEvaluation;

public interface SubjectEvaluationDao extends TypedDao<SubjectEvaluation> {

	Collection<SubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
}
