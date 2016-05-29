package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class StudentClassroomSessionDivisionDaoImpl extends AbstractTypedDao<StudentClassroomSessionDivision> implements StudentClassroomSessionDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readByStudentClassroomSessionDivision,readByStudentByClassroomSessionDivision,readByClassroomSession
    	,readByClassroomSessionDivisions,readByClassroomSessions,readByStudentByClassroomSession,readByClassroomSessionDivisionIndex
    	,readByClassroomSessionByTeacher;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentClassroomSessionDivision, _select().where(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION));
        
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION
        		, StudentClassroomSession.FIELD_CLASSROOMSESSION),StudentClassroomSession.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION));
        registerNamedQuery(readByStudentByClassroomSessionDivision, _select().where(StudentClassroomSessionDivision.FIELD_STUDENT)
        		.and(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION));
        
        registerNamedQuery(readByClassroomSessionDivisionIndex, _select()
        		.where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION, ClassroomSessionDivision.FIELD_INDEX), ClassroomSessionDivision.FIELD_INDEX));
        
        registerNamedQuery(readByStudentByClassroomSession, _select().where(StudentClassroomSessionDivision.FIELD_STUDENT)
        		.and(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION, StudentClassroomSession.FIELD_CLASSROOMSESSION)
        				, StudentClassroomSession.FIELD_CLASSROOMSESSION,ArithmeticOperator.EQ));
        
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION
        		, StudentClassroomSession.FIELD_CLASSROOMSESSION)));
        
        registerNamedQuery(readByClassroomSessionByTeacher, "SELECT r FROM StudentClassroomSessionDivision r WHERE r.classroomSessionDivision.classroomSession = :classroomSession AND "
        		+ "EXISTS( SELECT rr FROM ClassroomSessionDivisionSubject rr WHERE rr.teacher = :teacher AND rr.classroomSessionDivision = r.classroomSessionDivision)");
    }
    
	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByStudentClassroomSessionDivision).parameter(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION, classroomSessionDivision).resultMany();
	}

	@Override
	public StudentClassroomSessionDivision readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByStudentByClassroomSessionDivision).
				parameter(StudentClassroomSessionDivision.FIELD_STUDENT, student).parameter(StudentClassroomSessionDivision.FIELD_CLASSROOMSESSIONDIVISION, classroomSessionDivision)
				.ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(StudentClassroomSession.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}

	
	@Override
	public Collection<StudentClassroomSessionDivision> readByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return namedQuery(readByStudentByClassroomSession).parameter(StudentClassroomSessionDivision.FIELD_STUDENT, student)
				.parameter(StudentClassroomSession.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionDivisionIndex(Byte classroomSessionDivisionIndex) {
		return namedQuery(readByClassroomSessionDivisionIndex).parameter(ClassroomSessionDivision.FIELD_INDEX, classroomSessionDivisionIndex).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher) {
		return namedQuery(readByClassroomSessionByTeacher).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}

}
 