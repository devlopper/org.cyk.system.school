package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class StudentClassroomSessionDivisionDaoImpl extends AbstractTypedDao<StudentClassroomSessionDivision> implements StudentClassroomSessionDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

    private String readByClassroomSessionDivisions,readByStudentByClassroomSessionDivision,readByClassroomSession
    	,readByClassroomSessions,readByStudentByClassroomSession,readByClassroomSessionDivisionOrderNumber
    	,readByClassroomSessionByTeacher,readByLevelTimeDivision,readByAcademicSession,readByAcademicSessionByClassroomSessionDivisionOrderNumber;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, StudentClassroomSession.FIELD_CLASSROOM_SESSION),StudentClassroomSession.FIELD_CLASSROOM_SESSION));
        
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION));
        
        registerNamedQuery(readByStudentByClassroomSessionDivision, _select().where(StudentClassroomSessionDivision.FIELD_STUDENT)
        		.and(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION));
        
        registerNamedQuery(readByClassroomSessionDivisionOrderNumber, _select()
        		.where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_ORDER_NUMBER), GlobalIdentifier.FIELD_ORDER_NUMBER));
        
        registerNamedQuery(readByAcademicSessionByClassroomSessionDivisionOrderNumber, _select()
        		.where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
                		, ClassroomSessionDivision.FIELD_CLASSROOMSESSION,ClassroomSession.FIELD_ACADEMIC_SESSION),ClassroomSession.FIELD_ACADEMIC_SESSION)
        		.and(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_GLOBAL_IDENTIFIER
        				,GlobalIdentifier.FIELD_ORDER_NUMBER), GlobalIdentifier.FIELD_ORDER_NUMBER,ArithmeticOperator.EQ));
        
        registerNamedQuery(readByStudentByClassroomSession, _select().where(StudentClassroomSessionDivision.FIELD_STUDENT)
        		.and(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION, StudentClassroomSession.FIELD_CLASSROOM_SESSION)
        				, StudentClassroomSession.FIELD_CLASSROOM_SESSION,ArithmeticOperator.EQ));
        
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, StudentClassroomSession.FIELD_CLASSROOM_SESSION)));
        
        registerNamedQuery(readByClassroomSessionByTeacher, "SELECT r FROM StudentClassroomSessionDivision r WHERE r.classroomSessionDivision.classroomSession = :classroomSession AND "
        		+ "EXISTS( SELECT rr FROM ClassroomSessionDivisionSubject rr WHERE rr.teacher = :teacher AND rr.classroomSessionDivision = r.classroomSessionDivision)");
    
        registerNamedQuery(readByLevelTimeDivision, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, ClassroomSessionDivision.FIELD_CLASSROOMSESSION,ClassroomSession.FIELD_LEVEL_TIME_DIVISION),ClassroomSession.FIELD_LEVEL_TIME_DIVISION));
        
        registerNamedQuery(readByAcademicSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, ClassroomSessionDivision.FIELD_CLASSROOMSESSION,ClassroomSession.FIELD_ACADEMIC_SESSION),ClassroomSession.FIELD_ACADEMIC_SESSION));
        
    }
    
	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return readByClassroomSessionDivisions(Arrays.asList(classroomSessionDivision));
	}

	@Override
	public StudentClassroomSessionDivision readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByStudentByClassroomSessionDivision).
				parameter(StudentClassroomSessionDivision.FIELD_STUDENT, student).parameter(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(StudentClassroomSession.FIELD_CLASSROOM_SESSION, classroomSession).resultMany();
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
				.parameter(StudentClassroomSession.FIELD_CLASSROOM_SESSION, classroomSession).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionDivisionIndex(Long classroomSessionDivisionOrderNumber) {
		return namedQuery(readByClassroomSessionDivisionOrderNumber).parameter(GlobalIdentifier.FIELD_ORDER_NUMBER, classroomSessionDivisionOrderNumber).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher) {
		return namedQuery(readByClassroomSessionByTeacher).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSessionDivision> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return namedQuery(readByLevelTimeDivision).parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION,levelTimeDivision).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByAcademicSession(AcademicSession academicSession) {
		return namedQuery(readByAcademicSession).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION,academicSession).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivision> readByAcademicSessionByClassroomSessionDivisionOrderNumber(AcademicSession academicSession, Long classroomSessionDivisionOrderNumber) {
		return namedQuery(readByAcademicSessionByClassroomSessionDivisionOrderNumber).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION,academicSession)
				.parameter(GlobalIdentifier.FIELD_ORDER_NUMBER, classroomSessionDivisionOrderNumber).resultMany();
	}

}
 