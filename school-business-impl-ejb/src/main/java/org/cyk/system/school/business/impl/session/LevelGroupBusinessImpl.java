package org.cyk.system.school.business.impl.session;

import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.pattern.tree.AbstractDataTreeBusinessImpl;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.persistence.api.session.LevelGroupDao;

public class LevelGroupBusinessImpl extends AbstractDataTreeBusinessImpl<LevelGroup,LevelGroupDao,LevelGroupType> implements LevelGroupBusiness {
 
	private static final long serialVersionUID = 2801588592108008404L;

	@Inject
    public LevelGroupBusinessImpl(LevelGroupDao dao) {
        super(dao);
    }

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<LevelGroup> findByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher) {
		return dao.readByAcademicSessionByTeacher(academicSession, teacher);
	}
	
	
	
}
