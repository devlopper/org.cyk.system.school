package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.Subject;

public interface ClassroomSessionSubjectDao extends TypedDao<ClassroomSessionSubject> {
	
	Collection<ClassroomSessionSubject> readBySubject(Subject subject);
	Collection<ClassroomSessionSubject> readByClassroomSession(ClassroomSession classroomSession);
	ClassroomSessionSubject readByClassroomSessionBySubject(ClassroomSession classroomSession,Subject subject);
	Collection<ClassroomSessionSubject> readByClassroomSessionByStudent(ClassroomSession classroomSession,Student student);
	Collection<ClassroomSessionSubject> readByClassroomSessionByRequired(ClassroomSession classroomSession,Boolean required);
	
}
