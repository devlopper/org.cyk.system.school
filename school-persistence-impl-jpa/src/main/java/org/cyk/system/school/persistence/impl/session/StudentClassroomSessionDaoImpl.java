package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;

public class StudentClassroomSessionDaoImpl extends AbstractTypedDao<StudentClassroomSession> implements StudentClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readByClassroomSession,readByStudentByClassroomSession,readByClassroomSessions,readByLevelTimeDivision;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByClassroomSession, _select().where(StudentClassroomSession.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByLevelTimeDivision, _select().where(commonUtils.attributePath(StudentClassroomSession.FIELD_CLASSROOMSESSION, ClassroomSession.FIELD_LEVEL_TIME_DIVISION), ClassroomSession.FIELD_LEVEL_TIME_DIVISION));
        registerNamedQuery(readByStudentByClassroomSession, _select().where(StudentClassroomSession.FIELD_STUDENT).and(StudentClassroomSession.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(StudentClassroomSession.FIELD_CLASSROOMSESSION));
    }
    
	@Override
	public Collection<StudentClassroomSession> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(StudentClassroomSession.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}

	@Override
	public StudentClassroomSession readByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return namedQuery(readByStudentByClassroomSession).
				parameter(StudentClassroomSession.FIELD_STUDENT, student).parameter(StudentClassroomSession.FIELD_CLASSROOMSESSION, classroomSession)
				.ignoreThrowable(NoResultException.class)
				.resultOne();
	}

	@Override
	public Collection<StudentClassroomSession> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return namedQuery(readByLevelTimeDivision).parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, levelTimeDivision).resultMany();
	}

}
 