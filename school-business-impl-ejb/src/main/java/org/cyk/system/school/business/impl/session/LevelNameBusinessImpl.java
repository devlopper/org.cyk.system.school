package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractEnumerationBusinessImpl;
import org.cyk.system.school.business.api.session.LevelNameBusiness;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.persistence.api.session.LevelNameDao;

public class LevelNameBusinessImpl extends AbstractEnumerationBusinessImpl<LevelName, LevelNameDao> implements LevelNameBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LevelNameBusinessImpl(LevelNameDao dao) {
		super(dao); 
	}   
	
}
