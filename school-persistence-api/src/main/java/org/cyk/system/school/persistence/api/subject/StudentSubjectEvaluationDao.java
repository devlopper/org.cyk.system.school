package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.SubjectEvaluation;

public interface StudentSubjectEvaluationDao extends TypedDao<StudentSubjectEvaluation> {

	Collection<StudentSubjectEvaluation> readByStudentSubject(StudentSubject studentSubject);
	Long countByStudentSubject(StudentSubject studentSubject);

	Collection<StudentSubjectEvaluation> readBySubjectEvaluation(SubjectEvaluation subjectEvaluation);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentSubjectEvaluation> readByClassroomSession(ClassroomSession classroomSession);
	Collection<StudentSubjectEvaluation> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	Collection<StudentSubjectEvaluation> readBySubjects(Collection<ClassroomSessionDivisionSubject> levels);
	Collection<StudentSubjectEvaluation> readByClassroomSessions(Collection<ClassroomSession> levels);
	Collection<StudentSubjectEvaluation> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);
	
}
