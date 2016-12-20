package org.cyk.system.school.persistence.impl.session;

import java.io.Serializable;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;

public class LevelTimeDivisionDaoImpl extends AbstractTypedDao<LevelTimeDivision> implements LevelTimeDivisionDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByLevelByTimeDivision;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByLevelByTimeDivision, _select().where(LevelTimeDivision.FIELD_LEVEL).and(LevelTimeDivision.FIELD_TIME_DIVISION_TYPE));
	}
	
	@Override
	public LevelTimeDivision readByLevelByTimeDivision(Level level, TimeDivisionType timeDivisionType) {
		return namedQuery(readByLevelByTimeDivision).parameter(LevelTimeDivision.FIELD_LEVEL, level).parameter(LevelTimeDivision.FIELD_TIME_DIVISION_TYPE, timeDivisionType)
				.ignoreThrowable(NoResultException.class).resultOne();
	}
	
}
 