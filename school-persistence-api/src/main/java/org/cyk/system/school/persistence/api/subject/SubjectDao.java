package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.Subject;

public interface SubjectDao extends TypedDao<Subject> {

	Collection<Subject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<Subject> readByClassroomSession(ClassroomSession classroomSession);

	Collection<Subject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);

	Collection<Subject> readByClassroomSessions(Collection<ClassroomSession> levels);
	
}
