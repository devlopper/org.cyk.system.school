package org.cyk.system.school.business.impl.session;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.pattern.tree.AbstractDataTreeTypeBusinessImpl;
import org.cyk.system.school.business.api.session.LevelGroupTypeBusiness;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.persistence.api.session.LevelGroupTypeDao;

public class LevelGroupTypeBusinessImpl extends AbstractDataTreeTypeBusinessImpl<LevelGroupType,LevelGroupTypeDao> implements LevelGroupTypeBusiness {

	private static final long serialVersionUID = -5970296090669949506L;

	@Inject
    public LevelGroupTypeBusinessImpl(LevelGroupTypeDao dao) {
        super(dao);
    } 

}
