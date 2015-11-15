package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;

public class ClassroomSessionDaoImpl extends AbstractTypedDao<ClassroomSession> implements ClassroomSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByAcademicSession;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByAcademicSession, _select().where(ClassroomSession.FIELD_ACADEMIC_SESSION));
	}
	
	@Override
	public Collection<ClassroomSession> readByAcademicSession(AcademicSession academicSession) {
		return namedQuery(readByAcademicSession).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession).resultMany();
	}
	
    
	
}
 