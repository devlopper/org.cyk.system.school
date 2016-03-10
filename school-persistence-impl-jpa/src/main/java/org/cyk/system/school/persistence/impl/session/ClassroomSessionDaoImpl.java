package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;

public class ClassroomSessionDaoImpl extends AbstractTypedDao<ClassroomSession> implements ClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByAcademicSession,readByAcademicSessionByTeacher,readByLevelTimeDivision;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByAcademicSession, _select().where(ClassroomSession.FIELD_ACADEMIC_SESSION));
		registerNamedQuery(readByAcademicSessionByTeacher, "SELECT aClassroomSession FROM ClassroomSession aClassroomSession WHERE aClassroomSession.academicSession=:academicSession AND EXISTS( "
				+ " SELECT aSubject FROM ClassroomSessionDivisionSubject aSubject WHERE aSubject.classroomSessionDivision.classroomSession=aClassroomSession AND aSubject.teacher=:teacher"
				+ " )");
		registerNamedQuery(readByLevelTimeDivision, _select().where(ClassroomSession.FIELD_LEVEL_TIME_DIVISION));
	}
	
	@Override
	public Collection<ClassroomSession> readByAcademicSession(AcademicSession academicSession) {
		return namedQuery(readByAcademicSession).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession).resultMany();
	}
	
	@Override
	public Collection<ClassroomSession> readByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher) {
		return namedQuery(readByAcademicSessionByTeacher).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}

	@Override
	public Collection<ClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return namedQuery(readByLevelTimeDivision).parameter(ClassroomSession.FIELD_LEVEL_TIME_DIVISION, levelTimeDivision).resultMany();
	}
	
    
	
}
 