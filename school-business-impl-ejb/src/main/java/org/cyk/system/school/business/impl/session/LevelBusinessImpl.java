package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.session.LevelBusiness;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.persistence.api.session.LevelDao;

@Stateless
public class LevelBusinessImpl extends AbstractTypedBusinessService<Level, LevelDao> implements LevelBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LevelBusinessImpl(LevelDao dao) {
		super(dao);  
	}
	
	@Override
	protected void setProperty(Level level, String name) {
		if(GlobalIdentifier.FIELD_CODE.equals(name))
			level.setCode(generateCode(level.getGroup().getCode(),level.getLevelName().getCode()));
	}
}
