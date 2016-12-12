package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.LevelNameDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class ClassroomSessionDaoImpl extends AbstractTypedDao<ClassroomSession> implements ClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByAcademicSession,readByAcademicSessionByTeacher,readByLevelTimeDivision,readByAcademicSessionByLevelTimeDivisionBySuffix
		,readByAcademicSessionByLevelGroup,readByAcademicSessionByLevelGroupByTeacher,readByLevelNameBySuffix,readByLevelName,readWhereSuffixIsNullByLevelName;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByAcademicSession, _select().where(ClassroomSession.FIELD_ACADEMIC_SESSION));
		registerNamedQuery(readByAcademicSessionByLevelTimeDivisionBySuffix, _select().where(ClassroomSession.FIELD_ACADEMIC_SESSION)
				.and(ClassroomSession.FIELD_LEVEL_TIME_DIVISION).and(ClassroomSession.FIELD_SUFFIX));
		registerNamedQuery(readByAcademicSessionByLevelGroup, _select().where(ClassroomSession.FIELD_ACADEMIC_SESSION)
				.and(commonUtils.attributePath(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, LevelTimeDivision.FIELD_LEVEL,Level.FIELD_GROUP),PARAMETER_GROUP,ArithmeticOperator.EQ));
		registerNamedQuery(readByAcademicSessionByTeacher, "SELECT aClassroomSession FROM ClassroomSession aClassroomSession WHERE aClassroomSession.academicSession=:academicSession AND EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision.classroomSession=aClassroomSession AND aSubject.teacher=:teacher"
				+ " )");
		registerNamedQuery(readByAcademicSessionByLevelGroupByTeacher, "SELECT aClassroomSession FROM ClassroomSession aClassroomSession WHERE aClassroomSession.academicSession=:academicSession AND "
				+ " aClassroomSession.levelTimeDivision.level.group = :"+PARAMETER_GROUP+" AND EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision.classroomSession=aClassroomSession AND aSubject.teacher=:teacher"
				+ " )");
		registerNamedQuery(readByLevelTimeDivision, _select().where(ClassroomSession.FIELD_LEVEL_TIME_DIVISION));
		registerNamedQuery(readByLevelNameBySuffix, "SELECT aClassroomSession"
        		+ " FROM ClassroomSession aClassroomSession WHERE "
        		+ " aClassroomSession.levelTimeDivision.level.levelName=:levelName"
        		+ " AND aClassroomSession.suffix = :suffix");
		registerNamedQuery(readWhereSuffixIsNullByLevelName, "SELECT aClassroomSession"
        		+ " FROM ClassroomSession aClassroomSession WHERE "
        		+ " aClassroomSession.levelTimeDivision.level.levelName=:levelName"
        		+ " AND aClassroomSession.suffix IS NULL");
		registerNamedQuery(readByLevelName, "SELECT aClassroomSession"
        		+ " FROM ClassroomSession aClassroomSession WHERE "
        		+ " aClassroomSession.levelTimeDivision.level.levelName=:levelName");
	}
	
	@Override
	public Collection<ClassroomSession> readByAcademicSession(AcademicSession academicSession) {
		return namedQuery(readByAcademicSession).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession).resultMany();
	}
	
	@Override
	public Collection<ClassroomSession> readByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher) {
		return namedQuery(readByAcademicSessionByTeacher).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}

	@Override
	public Collection<ClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return namedQuery(readByLevelTimeDivision).parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, levelTimeDivision).resultMany();
	}
	
    @Override
    public ClassroomSession readByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, String suffix) {
    	return namedQuery(readByAcademicSessionByLevelTimeDivisionBySuffix).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession)
    			.parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, academicSession).parameter(ClassroomSession.FIELD_SUFFIX, academicSession).resultOne();
    }

	@Override
	public Collection<ClassroomSession> readByAcademicSessionByLevelGroup(AcademicSession academicSession, LevelGroup levelGroup) {
		return namedQuery(readByAcademicSessionByLevelGroup).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession)
				.parameter(PARAMETER_GROUP, levelGroup).resultMany();
	}
	
	@Override
	public Collection<ClassroomSession> readByAcademicSessionByLevelGroupByTeacher(AcademicSession academicSession, LevelGroup levelGroup,Teacher teacher) {
		return namedQuery(readByAcademicSessionByLevelGroupByTeacher).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession)
				.parameter(PARAMETER_GROUP, levelGroup).parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}
	
	@Override
	public Collection<ClassroomSession> readByLevelNameBySuffix(String levelNameCode, String suffix) {
		return namedQuery(readByLevelNameBySuffix).parameter(Level.FIELD_LEVEL_NAME, inject(LevelNameDao.class).read(levelNameCode))
				.parameter(ClassroomSession.FIELD_SUFFIX, suffix).resultMany();
	}
	
	@Override
	public Collection<ClassroomSession> readByLevelName(String levelNameCode) {
		return namedQuery(readByLevelNameBySuffix).parameter(Level.FIELD_LEVEL_NAME, inject(LevelNameDao.class).read(levelNameCode))
				.resultMany();
	}
	
	@Override
	public Collection<ClassroomSession> readWhereSuffixIsNullByLevelName(String levelNameCode) {
		return namedQuery(readWhereSuffixIsNullByLevelName).parameter(Level.FIELD_LEVEL_NAME, inject(LevelNameDao.class).read(levelNameCode))
				.resultMany();
	}
	
	private static final String PARAMETER_GROUP = "pgroup";

	

	
}
 