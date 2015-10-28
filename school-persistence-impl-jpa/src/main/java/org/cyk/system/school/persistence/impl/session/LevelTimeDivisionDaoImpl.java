package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;

public class LevelTimeDivisionDaoImpl extends AbstractTypedDao<LevelTimeDivision> implements LevelTimeDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	//private String readByClassroomSession;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		//registerNamedQuery(readByClassroomSession, _select().where(ClassroomSession.FIELD_LEVEL_TIME_DIVISION));
	}
	
	@Override
	public Collection<LevelTimeDivision> readByClassroomSession(ClassroomSession classroomSession) {
		return null;//namedQuery(readByClassroomSession).parameter(School.FIELD_OWNED_COMPANY, ownedCompany).ignoreThrowable(NoResultException.class).result;
	}
	
}
 