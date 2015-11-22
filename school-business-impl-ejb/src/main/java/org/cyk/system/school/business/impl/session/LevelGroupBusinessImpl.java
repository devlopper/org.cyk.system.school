package org.cyk.system.school.business.impl.session;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.pattern.tree.AbstractDataTreeBusinessImpl;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.persistence.api.session.LevelGroupDao;

public class LevelGroupBusinessImpl extends AbstractDataTreeBusinessImpl<LevelGroup,LevelGroupDao,LevelGroupType> implements LevelGroupBusiness {
 
	private static final long serialVersionUID = 2801588592108008404L;

	@Inject
    public LevelGroupBusinessImpl(LevelGroupDao dao) {
        super(dao);
    }
	
}
