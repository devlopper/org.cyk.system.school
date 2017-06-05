package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface ClassroomSessionDivisionSubjectEvaluationTypeBusiness extends TypedBusiness<ClassroomSessionDivisionSubjectEvaluationType> {

	ClassroomSessionDivisionSubjectEvaluationType instancaiteOne(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType);
	
	ClassroomSessionDivisionSubjectEvaluationType findByClassroomSessionDivisionSubjectByEvaluationType(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType);

	Collection<ClassroomSessionDivisionSubjectEvaluationType> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
}
