package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;

public interface LevelDao extends TypedDao<Level> {

	Collection<Level> readByLevelName(LevelName levelName);
	
}
