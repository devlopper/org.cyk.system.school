package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;

public interface StudentClassroomSessionSubjectDao extends TypedDao<StudentClassroomSessionSubject> {

	Collection<StudentClassroomSessionSubject> readByStudentByClassroomSession(Student student, ClassroomSession classroomSession);
	Collection<StudentClassroomSessionSubject> readByStudentClassroomSession(StudentClassroomSession studentClassroomSession);

}
