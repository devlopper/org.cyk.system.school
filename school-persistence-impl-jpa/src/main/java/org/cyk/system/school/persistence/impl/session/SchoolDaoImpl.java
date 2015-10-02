package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;

import javax.persistence.NoResultException;

import org.cyk.system.company.model.structure.OwnedCompany;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.SchoolDao;

public class SchoolDaoImpl extends AbstractTypedDao<School> implements SchoolDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByOwnedCompany;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByOwnedCompany, _select().where(School.FIELD_OWNED_COMPANY));
	}
	
	@Override
	public School readByOwnedCompany(OwnedCompany ownedCompany) {
		return namedQuery(readByOwnedCompany).parameter(School.FIELD_OWNED_COMPANY, ownedCompany).ignoreThrowable(NoResultException.class).resultOne();
	}
	
}
 