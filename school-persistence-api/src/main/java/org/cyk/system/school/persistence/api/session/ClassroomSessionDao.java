package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface ClassroomSessionDao extends TypedDao<ClassroomSession> {

	Collection<ClassroomSession> readByAcademicSession(AcademicSession academicSession);
	Collection<ClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision);
	Collection<ClassroomSession> readByAcademicSessionByTeacher(AcademicSession academicSession, Teacher teacher);
	ClassroomSession readByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, String suffix);
	Collection<ClassroomSession> readByAcademicSessionByLevelGroup(AcademicSession academicSession, LevelGroup levelGroup);

}
