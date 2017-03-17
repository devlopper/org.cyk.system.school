package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface ClassroomSessionDivisionDao extends TypedDao<ClassroomSessionDivision> {

	Collection<ClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession);

	Collection<ClassroomSessionDivision> readByClassroomSessions(Collection<ClassroomSession> levels);

	Long countByClassroomSession(ClassroomSession classroomSession);

	ClassroomSessionDivision readByClassroomSessionByOrderNumber(ClassroomSession classroomSession, Long orderNumber);
	ClassroomSessionDivision readByClassroomSessionByIndexByTeacher(ClassroomSession classroomSession, Byte index,Teacher teacher);
	Collection<ClassroomSessionDivision> readByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher);
	Collection<ClassroomSessionDivision> readByClassroomSessionsByOrderNumber(Collection<ClassroomSession> classroomSessions, Long orderNumber);

	Collection<ClassroomSessionDivision> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision);

	Collection<ClassroomSessionDivision> readByLevelNameByClassroomSessionDivisionOrderNumber(String levelNameCode,Long classroomSessionDivisionOrderNumber);
	Collection<ClassroomSessionDivision> readByLevelNameByClassroomSessionSuffixByClassroomSessionDivisionOrderNumber(String levelNameCode,String classroomSessionSuffixCode,Long classroomSessionDivisionOrderNumber);

	Collection<ClassroomSessionDivision> readByLevelTimeDivisionByClassroomSessionDivisionSuffixByClassroomSessionDivisionOrderNumber(
			LevelTimeDivision levelTimeDivision, ClassroomSessionSuffix classroomSessionSuffix, Long classroomSessionDivisionOrderNumber);

	Collection<ClassroomSessionDivision> readByLevelTimeDivisionByClassroomSessionDivisionOrderNumber(
			LevelTimeDivision levelTimeDivision, Long classroomSessionDivisionOrderNumber);

}
