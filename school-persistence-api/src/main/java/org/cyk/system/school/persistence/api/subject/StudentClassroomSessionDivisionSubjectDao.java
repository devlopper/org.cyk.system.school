package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;

public interface StudentClassroomSessionDivisionSubjectDao extends TypedDao<StudentClassroomSessionDivisionSubject> /*AbstractStudentResultsDao<ClassroomSessionDivisionSubject,StudentSubject,StudentSubjectEvaluation>*/ {

	StudentClassroomSessionDivisionSubject readByStudentBySubject(Student student, ClassroomSessionDivisionSubject subject);
	Collection<StudentClassroomSessionDivisionSubject> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject);
	
	Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentClassroomSessionDivisionSubject> readByClassroomSession(ClassroomSession classroomSession);

	Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);

	Collection<StudentClassroomSessionDivisionSubject> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects);

	Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessions(Collection<ClassroomSession> levels);

	Collection<StudentClassroomSessionDivisionSubject> readByStudent(Student student);
	Collection<StudentClassroomSessionDivisionSubject> readByStudentByClassroomSession(Student student,ClassroomSession classroomSession);
	Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision, Teacher teacher);
	
}
