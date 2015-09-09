package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;

public class StudentClassroomSessionDaoImpl extends AbstractTypedDao<StudentClassroomSession> implements StudentClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readByClassroomSession,readByStudentByClassroomSession,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByClassroomSession, _select().where("classroomSession"));
        registerNamedQuery(readByStudentByClassroomSession, _select().where("student").and("classroomSession"));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn("classroomSession"));
    }
    
	@Override
	public Collection<StudentClassroomSession> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter("classroomSession", classroomSession).resultMany();
	}

	@Override
	public StudentClassroomSession readByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return namedQuery(readByStudentByClassroomSession).
				parameter("student", student).parameter("classroomSession", classroomSession)
				.ignoreThrowable(NoResultException.class)
				.resultOne();
	}

	@Override
	public Collection<StudentClassroomSession> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}

}
 