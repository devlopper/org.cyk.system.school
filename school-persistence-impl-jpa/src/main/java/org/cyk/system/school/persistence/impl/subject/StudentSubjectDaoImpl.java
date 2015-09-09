package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.utility.common.computation.ArithmeticOperator;
import org.cyk.utility.common.computation.LogicalOperator;

public class StudentSubjectDaoImpl extends AbstractTypedDao<StudentSubject> implements StudentSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
   private String readByStudentBySubject,readBySubject,readByClassroomSessionDivision,readByClassroomSession,readByClassroomSessionDivisions,readBySubjects,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentBySubject, _select().where("student").where(LogicalOperator.AND,"subject","subject",ArithmeticOperator.EQ));
        registerNamedQuery(readBySubject, _select().where("subject"));
        registerNamedQuery(readByClassroomSessionDivision, _select().where("subject.classroomSessionDivision","classroomSessionDivision"));
        registerNamedQuery(readByClassroomSession, _select().where("subject.classroomSessionDivision.classroomSession","classroomSession"));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn("subject.classroomSessionDivision"));
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn("subject"));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn("subject.classroomSessionDivision.classroomSession"));
        
    }
    
    @Override
    public StudentSubject readByStudentBySubject(Student student,Subject subject) {
        return namedQuery(readByStudentBySubject).parameter("student", student).parameter("subject", subject).ignoreThrowable(NoResultException.class)
                .resultOne();
    }

	@Override
	public Collection<StudentSubject> readBySubject(Subject subject) {
		return namedQuery(readBySubject).parameter("subject", subject).resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter("classroomSessionDivision", classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter("classroomSession", classroomSession).resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<StudentSubject> readBySubjects(Collection<Subject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}
}
 