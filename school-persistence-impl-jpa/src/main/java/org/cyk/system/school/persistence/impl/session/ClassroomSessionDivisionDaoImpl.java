package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class ClassroomSessionDivisionDaoImpl extends AbstractTypedDao<ClassroomSessionDivision> implements ClassroomSessionDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByClassroomSession,readByClassroomSessions,countByClassroomSession,readByClassroomSessionByIndex;
     
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation(); 
        registerNamedQuery(readByClassroomSession, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionByIndex, _select().where(ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(ClassroomSessionDivision.FIELD_INDEX, PARAMETER_INDEX,ArithmeticOperator.EQ));
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

	@Override
	public Long countByClassroomSession(ClassroomSession classroomSession) {
		return countNamedQuery(countByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
                .resultOne();
	}
	
	@Override
	public ClassroomSessionDivision readByClassroomSessionByIndex(ClassroomSession classroomSession, Byte index) {
		return namedQuery(readByClassroomSessionByIndex).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(PARAMETER_INDEX, index)
                .resultOne();
	}
	
}
 