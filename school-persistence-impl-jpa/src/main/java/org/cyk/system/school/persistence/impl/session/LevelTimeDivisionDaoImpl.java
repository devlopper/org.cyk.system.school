package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;

public class LevelTimeDivisionDaoImpl extends AbstractTypedDao<LevelTimeDivision> implements LevelTimeDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByIndex;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByIndex, _select().where(LevelTimeDivision.FIELD_INDEX,PARAMETER_INDEX));
	}
	
	@Override 
	public Collection<LevelTimeDivision> readByIndex(Integer index) {
		return namedQuery(readByIndex).parameter(PARAMETER_INDEX, index).resultMany();
	}
	
	/**/
	
}
 