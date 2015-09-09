package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;

public interface StudentClassroomSessionDao extends TypedDao<StudentClassroomSession> {

	Collection<StudentClassroomSession> readByClassroomSession(ClassroomSession classroomSession);

	StudentClassroomSession readByStudentByClassroomSession(Student student, ClassroomSession classroomSession);

	Collection<StudentClassroomSession> readByClassroomSessions(Collection<ClassroomSession> levels);

}
