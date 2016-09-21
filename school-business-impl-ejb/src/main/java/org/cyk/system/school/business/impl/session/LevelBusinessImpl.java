package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
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
	protected Object[] getPropertyValueTokens(Level level, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{level.getGroup(),level.getLevelName(),level.getSpeciality()};
		return super.getPropertyValueTokens(level, name);
	}

}
