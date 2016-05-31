package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class ClassroomSessionDivisionDaoImpl extends AbstractTypedDao<ClassroomSessionDivision> implements ClassroomSessionDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByClassroomSession,readByClassroomSessions,countByClassroomSession,readByClassroomSessionByIndex,readByClassroomSessionByIndexByTeacher
    	,readByClassroomSessionByTeacher,readByClassroomSessionsByIndex,readByLevelTimeDivision;
     
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation(); 
        registerNamedQuery(readByClassroomSession, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionByTeacher, "SELECT aClassroomSessionDivision FROM ClassroomSessionDivision aClassroomSessionDivision WHERE aClassroomSessionDivision.classroomSession=:classroomSession AND EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision=aClassroomSessionDivision AND aSubject.teacher=:teacher"
				+ " )");
        registerNamedQuery(readByClassroomSessionByIndexByTeacher, "SELECT aClassroomSessionDivision FROM ClassroomSessionDivision aClassroomSessionDivision WHERE "
        		+ " aClassroomSessionDivision.classroomSession=:classroomSession AND aClassroomSessionDivision.index=:pindex AND EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision=aClassroomSessionDivision AND aSubject.teacher=:teacher"
				+ " )");
        registerNamedQuery(readByLevelTimeDivision, "SELECT aClassroomSessionDivision FROM ClassroomSessionDivision aClassroomSessionDivision WHERE "
        		+ " aClassroomSessionDivision.classroomSession.levelTimeDivision=:levelTimeDivision ");
        registerNamedQuery(readByClassroomSessionByIndex, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(ClassroomSessionDivision.FIELD_INDEX, PARAMETER_INDEX,ArithmeticOperator.EQ));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionsByIndex, _select().whereIdentifierIn(ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(ClassroomSessionDivision.FIELD_INDEX, PARAMETER_INDEX,ArithmeticOperator.EQ));
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
	public ClassroomSessionDivision readByClassroomSessionByIndex(ClassroomSession classroomSession, Byte index) {
		return namedQuery(readByClassroomSessionByIndex).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(PARAMETER_INDEX, index).ignoreThrowable(NoResultException.class)
                .resultOne();
	}
	
	@Override
	public Collection<ClassroomSessionDivision> readByClassroomSessionsByIndex(Collection<ClassroomSession> classroomSessions, Byte index) {
		return namedQuery(readByClassroomSessionsByIndex).parameterIdentifiers(classroomSessions).parameter(PARAMETER_INDEX, index)
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
	
}
 