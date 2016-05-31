package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface ClassroomSessionDivisionDao extends TypedDao<ClassroomSessionDivision> {

	Collection<ClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession);

	Collection<ClassroomSessionDivision> readByClassroomSessions(Collection<ClassroomSession> levels);

	Long countByClassroomSession(ClassroomSession classroomSession);

	ClassroomSessionDivision readByClassroomSessionByIndex(ClassroomSession classroomSession, Byte index);
	ClassroomSessionDivision readByClassroomSessionByIndexByTeacher(ClassroomSession classroomSession, Byte index,Teacher teacher);
	Collection<ClassroomSessionDivision> readByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher);
	Collection<ClassroomSessionDivision> readByClassroomSessionsByIndex(Collection<ClassroomSession> classroomSessions, Byte index);

	Collection<ClassroomSessionDivision> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision);

}
