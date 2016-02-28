package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface LevelTimeDivisionDao extends TypedDao<LevelTimeDivision> {
	
	Collection<LevelTimeDivision> readByIndex(Integer index);

}
