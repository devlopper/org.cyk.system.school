package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.business.api.pattern.tree.AbstractDataTreeBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;

public interface LevelGroupBusiness extends AbstractDataTreeBusiness<LevelGroup,LevelGroupType> {

	Collection<LevelGroup> findByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher);
	
}
