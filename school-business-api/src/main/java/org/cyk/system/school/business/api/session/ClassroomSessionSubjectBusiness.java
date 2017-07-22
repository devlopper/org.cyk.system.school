package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.Subject;

public interface ClassroomSessionSubjectBusiness extends TypedBusiness<ClassroomSessionSubject> {

	ClassroomSessionSubject instanciateOne(Subject subject,ClassroomSession classroomSession);
	Collection<ClassroomSessionSubject> findBySubject(Subject subject);
	Collection<ClassroomSessionSubject> findByClassroomSession(ClassroomSession classroomSession);
	ClassroomSessionSubject findByClassroomSessionBySubject(ClassroomSession classroomSession,Subject subject);
	Collection<ClassroomSessionSubject> findByClassroomSessionByStudent(ClassroomSession classroomSession,Student student);
	
}
