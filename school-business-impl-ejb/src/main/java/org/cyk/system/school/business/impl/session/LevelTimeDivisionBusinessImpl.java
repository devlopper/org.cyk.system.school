package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.session.LevelTimeDivisionBusiness;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;

@Stateless
public class LevelTimeDivisionBusinessImpl extends AbstractTypedBusinessService<LevelTimeDivision, LevelTimeDivisionDao> implements LevelTimeDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public LevelTimeDivisionBusinessImpl(LevelTimeDivisionDao dao) {
		super(dao);  
	}
	
	@Override
	protected Object[] getPropertyValueTokens(LevelTimeDivision levelTimeDivision, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{levelTimeDivision.getLevel(),levelTimeDivision.getTimeDivisionType()};
		return super.getPropertyValueTokens(levelTimeDivision, name);
	}
	
	
}
