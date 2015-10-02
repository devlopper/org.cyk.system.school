package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;

public class ClassroomSessionDivisionDaoImpl extends AbstractTypedDao<ClassroomSessionDivision> implements ClassroomSessionDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByClassroomSession,readByClassroomSessions;
     
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation(); 
        registerNamedQuery(readByClassroomSession, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
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
	
}
 