package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class ClassroomSessionSubjectDaoImpl extends AbstractTypedDao<ClassroomSessionSubject> implements ClassroomSessionSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readBySubject,readByClassroomSession,countByClassroomSession,readBySubjectByClassroomSession,readByClassroomSessionByStudent,readByClassroomSessionByRequired;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readBySubject, _select().where(ClassroomSessionSubject.FIELD_SUBJECT));
		registerNamedQuery(readByClassroomSessionByStudent, "SELECT classroomSessionSubject FROM ClassroomSessionSubject classroomSessionSubject WHERE"
				+ " classroomSessionSubject.classroomSession = :classroomSession AND EXISTS( "
				+ "SELECT studentClassroomSessionDivisionSubject FROM StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject WHERE "
				+ "studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject.classroomSessionDivision.classroomSession = :classroomSession"
				+ " AND studentClassroomSessionDivisionSubject.classroomSessionDivisionSubject.subject = classroomSessionSubject.subject AND studentClassroomSessionDivisionSubject.student = :student)");
		registerNamedQuery(readByClassroomSession, _select().where(ClassroomSessionSubject.FIELD_CLASSROOMSESSION));
		registerNamedQuery(readByClassroomSessionByRequired, _select().where(ClassroomSessionSubject.FIELD_CLASSROOMSESSION).and("globalIdentifier.required", GlobalIdentifier.FIELD_REQUIRED, ArithmeticOperator.EQ));
		registerNamedQuery(readBySubjectByClassroomSession, _select().where(ClassroomSessionSubject.FIELD_SUBJECT).and(ClassroomSessionSubject.FIELD_CLASSROOMSESSION));
	}
	
	@Override
	public Collection<ClassroomSessionSubject> readBySubject(Subject subject) {
		return namedQuery(readBySubject).parameter(ClassroomSessionSubject.FIELD_SUBJECT, subject).resultMany();
	}

	@Override
	public Collection<ClassroomSessionSubject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionSubject.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}
	
	@Override
	public Long countByClassroomSession(ClassroomSession classroomSession) {
		return countNamedQuery(countByClassroomSession).parameter(ClassroomSessionSubject.FIELD_CLASSROOMSESSION, classroomSession).resultOne();
	}

	@Override
	public ClassroomSessionSubject readByClassroomSessionBySubject(ClassroomSession classroomSession, Subject subject) {
		return namedQuery(readBySubjectByClassroomSession).parameter(ClassroomSessionSubject.FIELD_SUBJECT, subject)
			.parameter(ClassroomSessionSubject.FIELD_CLASSROOMSESSION, classroomSession).ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<ClassroomSessionSubject> readByClassroomSessionByStudent(ClassroomSession classroomSession,Student student) {
		return namedQuery(readByClassroomSessionByStudent).parameter(StudentClassroomSession.FIELD_CLASSROOM_SESSION, classroomSession)
				.parameter(StudentClassroomSession.FIELD_STUDENT, student).resultMany();
	}

	@Override
	public Collection<ClassroomSessionSubject> readByClassroomSessionByRequired(ClassroomSession classroomSession,Boolean required) {
		return namedQuery(readByClassroomSessionByRequired).parameter(ClassroomSessionSubject.FIELD_CLASSROOMSESSION, classroomSession)
				.parameter(GlobalIdentifier.FIELD_REQUIRED, required).resultMany();
	}
	
}
 