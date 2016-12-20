package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.Subject;

public interface SubjectClassroomSessionDao extends TypedDao<SubjectClassroomSession> {
	
	Collection<SubjectClassroomSession> readBySubject(Subject subject);
	Collection<SubjectClassroomSession> readByClassroomSession(ClassroomSession classroomSession);
	SubjectClassroomSession readBySubjectByClassroomSession(Subject subject,ClassroomSession classroomSession);
	Collection<SubjectClassroomSession> readByClassroomSessionByStudent(ClassroomSession classroomSession,Student student);
	
}
