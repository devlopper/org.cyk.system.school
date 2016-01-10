package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;

public interface StudentSubjectEvaluationDao extends TypedDao<StudentSubjectEvaluation> {

	Collection<StudentSubjectEvaluation> readByStudentSubject(StudentSubject studentSubject);
	Long countByStudentSubject(StudentSubject studentSubject);

	Collection<StudentSubjectEvaluation> readByEvaluation(Evaluation evaluation);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentSubjectEvaluation> readByClassroomSession(ClassroomSession classroomSession);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivisionSubjects(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects);
	Collection<StudentSubjectEvaluation> readByClassroomSessions(Collection<ClassroomSession> classroomSessions);
	Collection<StudentSubjectEvaluation> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);
	
}
