package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface ClassroomSessionDivisionSubjectEvaluationTypeDao extends TypedDao<ClassroomSessionDivisionSubjectEvaluationType> {

	ClassroomSessionDivisionSubjectEvaluationType readByClassroomSessionDivisionSubjectByEvaluationType(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType);

	Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);

	Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivisionSubjects(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects1);
	
}
