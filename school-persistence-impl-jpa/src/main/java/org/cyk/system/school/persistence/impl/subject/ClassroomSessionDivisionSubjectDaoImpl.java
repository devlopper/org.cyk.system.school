package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

public class ClassroomSessionDivisionSubjectDaoImpl extends AbstractTypedDao<ClassroomSessionDivisionSubject> implements ClassroomSessionDivisionSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

   private String readByClassroomSessionDivision,readByClassroomSession,readByClassroomSessionDivisions,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByClassroomSessionDivision, _select().where(ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION));
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION
        		, StudentClassroomSession.FIELD_CLASSROOMSESSION),ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION
        		, StudentClassroomSession.FIELD_CLASSROOMSESSION)));
    }
    
    @Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
    	return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION, classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}
    
	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(classroomSessions).resultMany();
	}
	
}
 