package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.persistence.api.session.LevelDao;

public class LevelDaoImpl extends AbstractTypedDao<Level> implements LevelDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByLevelName;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByLevelName, _select().where(Level.FIELD_LEVEL_NAME));
	}
	
	@Override
	public Collection<Level> readByLevelName(LevelName levelName) {
		return namedQuery(readByLevelName).parameter(Level.FIELD_LEVEL_NAME, levelName).resultMany();
	}
	
}
 