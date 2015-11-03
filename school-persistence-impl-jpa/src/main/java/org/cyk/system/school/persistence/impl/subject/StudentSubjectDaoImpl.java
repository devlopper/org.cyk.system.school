package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.utility.common.computation.ArithmeticOperator;
import org.cyk.utility.common.computation.LogicalOperator;

public class StudentSubjectDaoImpl extends AbstractTypedDao<StudentSubject> implements StudentSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByStudentBySubject,readBySubject,readByClassroomSessionDivision,readByClassroomSession,readByClassroomSessionDivisions,readBySubjects,readByClassroomSessions;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentBySubject, _select().where(StudentSubject.FIELD_STUDENT).where(LogicalOperator.AND,StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT
        		,StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT,ArithmeticOperator.EQ));
        registerNamedQuery(readBySubject, _select().where(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT));
        registerNamedQuery(readByClassroomSessionDivision, _select().where(commonUtils.attributePath(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION)
        		,ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION));
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		,ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(commonUtils.attributePath(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION)
        		));
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT
        		, ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)));
        
    }
    
    @Override
    public StudentSubject readByStudentBySubject(Student student,ClassroomSessionDivisionSubject subject) {
        return namedQuery(readByStudentBySubject).parameter(StudentSubject.FIELD_STUDENT, student).parameter(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT, subject).ignoreThrowable(NoResultException.class)
                .resultOne();
    }

	@Override
	public Collection<StudentSubject> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject) {
		return namedQuery(readBySubject).parameter(StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT, subject).resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION, classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<StudentSubject> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<StudentSubject> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}
}
 