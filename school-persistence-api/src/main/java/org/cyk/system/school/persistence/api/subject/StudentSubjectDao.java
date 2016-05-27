package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface StudentSubjectDao extends TypedDao<StudentSubject> {

	StudentSubject readByStudentBySubject(Student student, ClassroomSessionDivisionSubject subject);
	Collection<StudentSubject> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentSubject> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject);
	
	Collection<StudentSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentSubject> readByClassroomSession(ClassroomSession classroomSession);

	Collection<StudentSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);

	Collection<StudentSubject> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects);

	Collection<StudentSubject> readByClassroomSessions(Collection<ClassroomSession> levels);

	Collection<StudentSubject> readByStudent(Student student);
	Collection<StudentSubject> readByStudentByClassroomSession(Student student,ClassroomSession classroomSession);
	Collection<StudentSubject> readByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision, Teacher teacher);
	
}
