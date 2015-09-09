package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.session.SchoolBusiness;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.persistence.api.session.SchoolDao;

public class SchoolBusinessImpl extends AbstractTypedBusinessService<School, SchoolDao> implements SchoolBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	
	@Inject
	public SchoolBusinessImpl(SchoolDao dao) {
		super(dao);  
	}

	@Override
	public School findDefault() {
		return dao.readByOwnedCompany(ownedCompanyBusiness.findDefaultOwnedCompany());
	}

}
