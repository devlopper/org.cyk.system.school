package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;

public interface StudentClassroomSessionDivisionSubjectEvaluationDao extends TypedDao<StudentClassroomSessionDivisionSubjectEvaluation> {

	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByStudentSubject(StudentClassroomSessionDivisionSubject studentSubject);
	Long countByStudentSubject(StudentClassroomSessionDivisionSubject studentSubject);

	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByEvaluation(Evaluation evaluation);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSession(ClassroomSession classroomSession);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivisionSubjects(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessions(Collection<ClassroomSession> classroomSessions);
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);
	
}
