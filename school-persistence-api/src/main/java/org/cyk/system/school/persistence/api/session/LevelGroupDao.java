package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.pattern.tree.AbstractDataTreeDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;

public interface LevelGroupDao extends AbstractDataTreeDao<LevelGroup,LevelGroupType> {

	Collection<LevelGroup> readByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher);

}
