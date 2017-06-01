package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.impl.event.AbstractIdentifiablePeriodDaoImpl;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.AcademicSessionDao;

public class AcademicSessionDaoImpl extends AbstractIdentifiablePeriodDaoImpl<AcademicSession> implements AcademicSessionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readDefaultedBySchool;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readDefaultedBySchool, _select().where(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER,GlobalIdentifier.FIELD_DEFAULTED)
				,GlobalIdentifier.FIELD_DEFAULTED).and(AcademicSession.FIELD_SCHOOL));
	}
	
	@Override
	public AcademicSession readDefaultedBySchool(School school) {
		return namedQuery(readDefaultedBySchool).parameter(GlobalIdentifier.FIELD_DEFAULTED, Boolean.TRUE)
				.parameter(AcademicSession.FIELD_SCHOOL, school).ignoreThrowable(NoResultException.class).resultOne();
	}
	
    
	
}
 