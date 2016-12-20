package org.cyk.system.school.business.api.session;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.Level;

public interface LevelBusiness extends TypedBusiness<Level> {

	Level instanciateOne(String levelGroupCode,String levelNameCode,String levelSpecialityCode);
	
}
