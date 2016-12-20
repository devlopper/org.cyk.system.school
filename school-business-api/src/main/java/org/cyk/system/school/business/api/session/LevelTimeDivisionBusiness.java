package org.cyk.system.school.business.api.session;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface LevelTimeDivisionBusiness extends TypedBusiness<LevelTimeDivision> {

	LevelTimeDivision instanciateOne(String code,String levelCode,String timeDivisionTypeCode,Long orderNumber);
	
	LevelTimeDivision findByLevelByTimeDivision(Level level,TimeDivisionType timeDivisionType);
	
}
