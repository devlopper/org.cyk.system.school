package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Subject;

public interface EvaluationDao extends TypedDao<Evaluation> {

	Collection<Evaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	Long countByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
	Collection<Evaluation> readByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType);
	Long countByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType);

	Collection<Evaluation> readByAcademicSessionByLevelByTimeDivisionTypeByClassroomSessionSuffixByDivisionOrderNumberBySubjectByEvaluationType(AcademicSession academicSession
			,Level level,TimeDivisionType timeDivisionType,ClassroomSessionSuffix classroomSessionSuffix,Long classroomSessionDivisionOrderNumber,Subject subject,EvaluationType evaluationType);
	
	Collection<Evaluation> readByClassroomSessionDivisionSubjectEvaluationTypes(Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes1);
	Long countByClassroomSessionDivisionSubjectEvaluationTypes(Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypes);

}
