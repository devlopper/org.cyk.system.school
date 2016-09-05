package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

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
	protected void setProperty(LevelTimeDivision levelTimeDivision, String name) {
		if(GlobalIdentifier.FIELD_CODE.equals(name))
			levelTimeDivision.setCode(generateCode(levelTimeDivision.getLevel().getCode(),levelTimeDivision.getTimeDivisionType().getCode()));
		super.setProperty(levelTimeDivision, name);
	}
}
