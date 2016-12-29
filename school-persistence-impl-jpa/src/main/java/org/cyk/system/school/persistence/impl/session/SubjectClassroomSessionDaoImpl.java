package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;

public class SubjectClassroomSessionDaoImpl extends AbstractTypedDao<SubjectClassroomSession> implements SubjectClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readBySubject,readByClassroomSession,readBySubjectByClassroomSession,readByClassroomSessionByStudent;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readBySubject, _select().where(SubjectClassroomSession.FIELD_SUBJECT));
		registerNamedQuery(readByClassroomSessionByStudent, "SELECT subjectClassroomSession FROM SubjectClassroomSession subjectClassroomSession WHERE"
				+ " subjectClassroomSession.classroomSession = :classroomSession AND EXISTS( "
				+ "SELECT studentClassroomSessionDivisionSubject FROM StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject WHERE "
				+ "studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject.classroomSessionDivision.classroomSession = :classroomSession"
				+ " AND studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject.subject = subjectClassroomSession.subject AND studentClassroomSessionDivisionSubject.student = :student)");
		registerNamedQuery(readByClassroomSession, _select().where(SubjectClassroomSession.FIELD_CLASSROOMSESSION));
		registerNamedQuery(readBySubjectByClassroomSession, _select().where(SubjectClassroomSession.FIELD_SUBJECT).and(SubjectClassroomSession.FIELD_CLASSROOMSESSION));
	}
	
	@Override
	public Collection<SubjectClassroomSession> readBySubject(Subject subject) {
		return namedQuery(readBySubject).parameter(SubjectClassroomSession.FIELD_SUBJECT, subject).resultMany();
	}

	@Override
	public Collection<SubjectClassroomSession> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(SubjectClassroomSession.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}

	@Override
	public SubjectClassroomSession readBySubjectByClassroomSession(Subject subject, ClassroomSession classroomSession) {
		return namedQuery(readBySubjectByClassroomSession).parameter(SubjectClassroomSession.FIELD_SUBJECT, subject)
			.parameter(SubjectClassroomSession.FIELD_CLASSROOMSESSION, classroomSession).ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<SubjectClassroomSession> readByClassroomSessionByStudent(ClassroomSession classroomSession,Student student) {
		return namedQuery(readByClassroomSessionByStudent).parameter(StudentClassroomSession.FIELD_CLASSROOM_SESSION, classroomSession)
				.parameter(StudentClassroomSession.FIELD_STUDENT, student).resultMany();
	}

	
	
}
 