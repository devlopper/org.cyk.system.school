package org.cyk.system.school.persistence.api.session;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface LevelTimeDivisionDao extends TypedDao<LevelTimeDivision> {

	LevelTimeDivision readByLevelByTimeDivision(Level level, TimeDivisionType timeDivisionType);
	
}
