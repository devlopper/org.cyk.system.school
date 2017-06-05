package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.Subject;

public interface SubjectClassroomSessionBusiness extends TypedBusiness<SubjectClassroomSession> {

	SubjectClassroomSession instanciateOne(Subject subject,ClassroomSession classroomSession);
	Collection<SubjectClassroomSession> findBySubject(Subject subject);
	Collection<SubjectClassroomSession> findByClassroomSession(ClassroomSession classroomSession);
	SubjectClassroomSession findBySubjectByClassroomSession(Subject subject,ClassroomSession classroomSession);
	Collection<SubjectClassroomSession> findByClassroomSessionByStudent(ClassroomSession classroomSession,Student student);
	
}
