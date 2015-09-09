package org.cyk.system.school.persistence.api.session;

import org.cyk.system.company.model.structure.OwnedCompany;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.School;

public interface SchoolDao extends TypedDao<School> {
	
	School readByOwnedCompany(OwnedCompany ownedCompany);

}
