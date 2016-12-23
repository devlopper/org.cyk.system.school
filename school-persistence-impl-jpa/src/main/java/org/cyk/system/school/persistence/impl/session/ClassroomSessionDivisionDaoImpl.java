package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.LevelNameDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class ClassroomSessionDivisionDaoImpl extends AbstractTypedDao<ClassroomSessionDivision> implements ClassroomSessionDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByClassroomSession,readByClassroomSessions,countByClassroomSession,readByClassroomSessionByOrderNumber,readByClassroomSessionByIndexByTeacher
    	,readByClassroomSessionByTeacher,readByClassroomSessionsByOrderNumber,readByLevelTimeDivision
    	,readByLevelNameByClassroomSessionDivisionOrderNumber,readByLevelNameByClassroomSessionSuffixByClassroomSessionDivisionOrderNumber;
     
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation(); 
        registerNamedQuery(readByClassroomSession, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionByTeacher, "SELECT aClassroomSessionDivision FROM ClassroomSessionDivision aClassroomSessionDivision WHERE"
        		+ " aClassroomSessionDivision.classroomSession=:classroomSession AND ( aClassroomSessionDivision.classroomSession.coordinator=:teacher OR EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision=aClassroomSessionDivision"
				+ " AND aSubject.teacher=:teacher)"
				+ " )");
        registerNamedQuery(readByClassroomSessionByIndexByTeacher, "SELECT aClassroomSessionDivision FROM ClassroomSessionDivision aClassroomSessionDivision WHERE "
        		+ " aClassroomSessionDivision.classroomSession=:classroomSession AND aClassroomSessionDivision.globalIdentifier.orderNumber=:pindex AND EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision=aClassroomSessionDivision"
				+ " AND aSubject.teacher=:teacher"
				+ " )");
        registerNamedQuery(readByLevelTimeDivision, "SELECT aClassroomSessionDivision FROM ClassroomSessionDivision aClassroomSessionDivision WHERE "
        		+ " aClassroomSessionDivision.classroomSession.levelTimeDivision=:levelTimeDivision ");
        registerNamedQuery(readByLevelNameByClassroomSessionDivisionOrderNumber, "SELECT aClassroomSessionDivision"
        		+ " FROM ClassroomSessionDivision aClassroomSessionDivision WHERE "
        		+ " aClassroomSessionDivision.classroomSession.levelTimeDivision.level.levelName=:levelName"
        		+ " AND aClassroomSessionDivision.globalIdentifier.orderNumber = :orderNumber");
        registerNamedQuery(readByLevelNameByClassroomSessionSuffixByClassroomSessionDivisionOrderNumber, "SELECT aClassroomSessionDivision"
        		+ " FROM ClassroomSessionDivision aClassroomSessionDivision WHERE "
        		+ " aClassroomSessionDivision.classroomSession.levelTimeDivision.level.levelName=:levelName"
        		+ " AND aClassroomSessionDivision.classroomSession.suffix = :suffix"
        		+ " AND aClassroomSessionDivision.globalIdentifier.orderNumber = :orderNumber");
        registerNamedQuery(readByClassroomSessionByOrderNumber, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(getGlobalIdentifierFieldPath(GlobalIdentifier.FIELD_ORDER_NUMBER), PARAMETER_INDEX,ArithmeticOperator.EQ));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionsByOrderNumber, _select().whereIdentifierIn(ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(getGlobalIdentifierFieldPath(GlobalIdentifier.FIELD_ORDER_NUMBER), PARAMETER_INDEX,ArithmeticOperator.EQ));
    } 
    
    @Override
	public Collection<ClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession) {
    	return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
                .resultMany();
	}

	@Override
	public Collection<ClassroomSessionDivision> readByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(classroomSessions)
                .resultMany();
	}
	
	@Override
	public Collection<ClassroomSessionDivision> readByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher) {
		return namedQuery(readByClassroomSessionByTeacher).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher)
                .resultMany();
	}

	@Override
	public Long countByClassroomSession(ClassroomSession classroomSession) {
		return countNamedQuery(countByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
                .resultOne();
	}
	
	@Override
	public ClassroomSessionDivision readByClassroomSessionByOrderNumber(ClassroomSession classroomSession, Long orderNumber) {
		return namedQuery(readByClassroomSessionByOrderNumber).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(PARAMETER_INDEX, orderNumber).ignoreThrowable(NoResultException.class)
                .resultOne();
	}
	
	@Override
	public Collection<ClassroomSessionDivision> readByClassroomSessionsByOrderNumber(Collection<ClassroomSession> classroomSessions, Long orderNumber) {
		return namedQuery(readByClassroomSessionsByOrderNumber).parameterIdentifiers(classroomSessions).parameter(PARAMETER_INDEX, orderNumber)
                .resultMany();
	}
	
	@Override
	public ClassroomSessionDivision readByClassroomSessionByIndexByTeacher(ClassroomSession classroomSession, Byte index,Teacher teacher) {
		return namedQuery(readByClassroomSessionByIndexByTeacher).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(PARAMETER_INDEX, index).parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher)
                .resultOne();
	}
	
	@Override
	public Collection<ClassroomSessionDivision> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return namedQuery(readByLevelTimeDivision).parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, levelTimeDivision)
                .resultMany();
	}
	
	@Override
	public Collection<ClassroomSessionDivision> readByLevelNameByClassroomSessionDivisionOrderNumber(String levelNameCode, Long classroomSessionDivisionOrderNumber) {
		return namedQuery(readByLevelNameByClassroomSessionDivisionOrderNumber).parameter(Level.FIELD_LEVEL_NAME, inject(LevelNameDao.class).read(levelNameCode))
				.parameter(GlobalIdentifier.FIELD_ORDER_NUMBER, classroomSessionDivisionOrderNumber)
                .resultMany();
	}
	
	@Override
	public Collection<ClassroomSessionDivision> readByLevelNameByClassroomSessionSuffixByClassroomSessionDivisionOrderNumber(String levelNameCode,String classroomSessionSuffix, Long classroomSessionDivisionOrderNumber) {
		return namedQuery(readByLevelNameByClassroomSessionSuffixByClassroomSessionDivisionOrderNumber)
				.parameter(Level.FIELD_LEVEL_NAME, inject(LevelNameDao.class).read(levelNameCode))
				.parameter(ClassroomSession.FIELD_SUFFIX, classroomSessionSuffix)
				.parameter(GlobalIdentifier.FIELD_ORDER_NUMBER, classroomSessionDivisionOrderNumber)
                .resultMany();
	}
	
}
 