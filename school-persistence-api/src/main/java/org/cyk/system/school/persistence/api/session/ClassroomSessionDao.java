package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSession.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface ClassroomSessionDao extends TypedDao<ClassroomSession> {

	Collection<ClassroomSession> readByAcademicSession(AcademicSession academicSession);
	Collection<ClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision);
	Collection<ClassroomSession> readByAcademicSessionByTeacher(AcademicSession academicSession, Teacher teacher);
	Collection<ClassroomSession> readByAcademicSessionByCoordinator(AcademicSession academicSession,Teacher coordinator);
	
	ClassroomSession readByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, ClassroomSessionSuffix suffix);
	ClassroomSession readWhereSuffixIsNullByAcademicSessionByLevelTimeDivision(AcademicSession academicSession,LevelTimeDivision levelTimeDivision);
	
	Collection<ClassroomSession> readByAcademicSessionByLevelGroup(AcademicSession academicSession, LevelGroup levelGroup);
	Collection<ClassroomSession> readByAcademicSessionByLevelGroupByTeacher(AcademicSession academicSession,LevelGroup levelGroup, Teacher teacher);
	Collection<ClassroomSession> readByLevelNameBySuffix(String levelNameCode, String suffixCode);
	Collection<ClassroomSession> readWhereSuffixIsNullByLevelName(String levelNameCode);
	Collection<ClassroomSession> readByLevelName(String levelNameCode);
	
	Collection<ClassroomSession> readByCriteria(SearchCriteria searchCriteria);
	Long countByCriteria(SearchCriteria searchCriteria);
	
}
