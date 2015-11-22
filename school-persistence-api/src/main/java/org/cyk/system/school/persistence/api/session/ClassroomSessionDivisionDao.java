package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;

public interface ClassroomSessionDivisionDao extends TypedDao<ClassroomSessionDivision> {

	Collection<ClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession);

	Collection<ClassroomSessionDivision> readByClassroomSessions(Collection<ClassroomSession> levels);

	Long countByClassroomSession(ClassroomSession classroomSession);

}
