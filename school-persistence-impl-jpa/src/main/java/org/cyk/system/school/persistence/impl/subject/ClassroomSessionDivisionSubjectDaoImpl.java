package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.root.persistence.impl.QueryWrapper;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

public class ClassroomSessionDivisionSubjectDaoImpl extends AbstractTypedDao<ClassroomSessionDivisionSubject> implements ClassroomSessionDivisionSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

   private String readByClassroomSessionDivision,readByClassroomSession,readByClassroomSessionDivisions,readByClassroomSessions,readByClassroomSessionDivisionByTeacher
   	,readByClassroomSessionBySubject,readByClassroomSessionDivisionBySubject,readWhereStudentExistByClassroomSessionDivision;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByClassroomSessionDivision, _select().where(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION));
        registerNamedQuery(readByClassroomSessionDivisionBySubject, _select().where(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		.and(ClassroomSessionDivisionSubject.FIELD_SUBJECT));
        registerNamedQuery(readByClassroomSessionDivisionByTeacher, _select().where(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		.and(ClassroomSessionDivisionSubject.FIELD_TEACHER));
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, StudentClassroomSession.FIELD_CLASSROOM_SESSION),ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionBySubject, _select().where(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, StudentClassroomSession.FIELD_CLASSROOM_SESSION),ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(ClassroomSessionDivisionSubject.FIELD_SUBJECT));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
        		, StudentClassroomSession.FIELD_CLASSROOM_SESSION)));
        
        registerNamedQuery(readWhereStudentExistByClassroomSessionDivision, _select().where(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		.and().whereString(" EXISTS( SELECT studentClassroomSessionDivisionSubject FROM StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject "
        				+ "WHERE studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject = r ) "));
        
    }
    
    @Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
    	return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}
	
	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessionBySubject(ClassroomSession classroomSession,Subject subject) {
		return namedQuery(readByClassroomSessionBySubject).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_SUBJECT, subject)
				.resultMany();
	}
    
	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(classroomSessions).resultMany();
	}
	
	@Override
	public Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision,Teacher teacher) {
		return namedQuery(readByClassroomSessionDivisionByTeacher).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher)
				.resultMany();
	}
	
	@Override
	public ClassroomSessionDivisionSubject readByClassroomSessionDivisionBySubject(ClassroomSessionDivision classroomSessionDivision,Subject subject) {
		return readByClassroomSessionDivisionBySubjectQuery(classroomSessionDivision, subject).ignoreThrowable(NoResultException.class).resultOne();
	}
	
	@Override
	public Collection<ClassroomSessionDivisionSubject> readManyByClassroomSessionDivisionBySubject(ClassroomSessionDivision classroomSessionDivision,Subject subject) {
		return readByClassroomSessionDivisionBySubjectQuery(classroomSessionDivision, subject).resultMany();
	}
	
	private QueryWrapper<ClassroomSessionDivisionSubject> readByClassroomSessionDivisionBySubjectQuery(ClassroomSessionDivision classroomSessionDivision,Subject subject) {
		return namedQuery(readByClassroomSessionDivisionBySubject).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.parameter(ClassroomSessionDivisionSubject.FIELD_SUBJECT, subject);
	}
	
	@Override
	public Collection<ClassroomSessionDivisionSubject> readDuplicates(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return readManyByClassroomSessionDivisionBySubject(classroomSessionDivisionSubject.getClassroomSessionDivision(), classroomSessionDivisionSubject.getSubject());
	}

	@Override
	public Collection<ClassroomSessionDivisionSubject> readWhereStudentExistByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readWhereStudentExistByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.resultMany();
	}
}
 