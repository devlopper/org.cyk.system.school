package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.EvaluatedStudentDao;

public class EvaluatedStudentDaoImpl extends AbstractTypedDao<EvaluatedStudent> implements EvaluatedStudentDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByStudentSubject,countByStudentSubject,readBySubject,readByClassroomSessionDivision,readByClassroomSession
    	,readByClassroomSessionDivisions,readBySubjects,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentSubject, _select().where("studentSubject", "studentSubject"));
        registerNamedQuery(readBySubject, _select().where("studentSubject.subject", "subject"));
        registerNamedQuery(readByClassroomSessionDivision, _select().where("studentSubject.subject.classroomSessionDivision", "classroomSessionDivision"));
        registerNamedQuery(readByClassroomSession, _select().where("studentSubject.subject.classroomSessionDivision.classroomSession", "classroomSession"));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn("evaluation.subject.classroomSessionDivision"));
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn("evaluation.subject"));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn("evaluation.subject.classroomSessionDivision.classroomSession"));
    }
    
	@Override
	public Collection<EvaluatedStudent> readByStudentSubject(StudentSubject studentSubject) {
		return namedQuery(readByStudentSubject).parameter("studentSubject", studentSubject)
                .resultMany();
	}
	
	@Override
	public Long countByStudentSubject(StudentSubject studentSubject) {
		return countNamedQuery(countByStudentSubject).parameter("studentSubject", studentSubject)
                .resultOne();
	}

	@Override
	public Collection<EvaluatedStudent> readBySubject(Subject subject) {
		return namedQuery(readBySubject).parameter("subject", subject)
                .resultMany();
	}

	@Override
	public Collection<EvaluatedStudent> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter("classroomSessionDivision", classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<EvaluatedStudent> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter("classroomSession", classroomSession)
                .resultMany();
	}
	
	@Override
	public Collection<EvaluatedStudent> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<EvaluatedStudent> readBySubjects(Collection<Subject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<EvaluatedStudent> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}
}
 