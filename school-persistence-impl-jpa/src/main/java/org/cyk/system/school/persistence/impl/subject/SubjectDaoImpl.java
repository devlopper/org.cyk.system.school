package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.SubjectDao;

public class SubjectDaoImpl extends AbstractTypedDao<Subject> implements SubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

   private String readByClassroomSessionDivision,readByClassroomSession,readByClassroomSessionDivisions,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByClassroomSessionDivision, _select().where("classroomSessionDivision"));
        registerNamedQuery(readByClassroomSession, _select().where("classroomSessionDivision.classroomSession","classroomSession"));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn("classroomSessionDivision"));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn("classroomSessionDivision.classroomSession"));
    }
    
    @Override
	public Collection<Subject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
    	return namedQuery(readByClassroomSessionDivision).parameter("classroomSessionDivision", classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<Subject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter("classroomSession", classroomSession).resultMany();
	}
    
	@Override
	public Collection<Subject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<Subject> readByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(classroomSessions).resultMany();
	}
	
}
 