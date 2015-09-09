package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;

public interface StudentSubjectDao extends TypedDao<StudentSubject> {

	StudentSubject readByStudentBySubject(Student student, Subject subject);

	Collection<StudentSubject> readBySubject(Subject subject);
	
	Collection<StudentSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentSubject> readByClassroomSession(
			ClassroomSession classroomSession);

	Collection<StudentSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);

	Collection<StudentSubject> readBySubjects(Collection<Subject> subjects);

	Collection<StudentSubject> readByClassroomSessions(Collection<ClassroomSession> levels);
	
}
