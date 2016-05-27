package org.cyk.system.school.persistence.impl.session;

import java.util.Collection;

import org.cyk.system.root.persistence.impl.pattern.tree.AbstractDataTreeDaoImpl;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.session.LevelGroupDao;

public class LevelGroupDaoImpl extends AbstractDataTreeDaoImpl<LevelGroup,LevelGroupType> implements LevelGroupDao {

	private static final long serialVersionUID = 6920278182318788380L;

	private String readByAcademicSessionByTeacher;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		/*registerNamedQuery(readByAcademicSessionByTeacher, "SELECT levelGroup FROM LevelGroup levelGroup WHERE "
				+ "EXISTS( SELECT lvl FROM Level lvl WHERE lvl.group = levelGroup AND "
				+ "EXISTS( SELECT ltd FROM LevelTimeDivision ltd WHERE ltd.level = lvl AND "
				+ "EXISTS( SELECT cs FROM ClassroomSession cs WHERE cs.levelTimeDivision = ltd AND cs.academicSession = :academicSession AND "
				+ "EXISTS( SELECT csd FROM ClassroomSessionDivision csd WHERE csd.classroomSession = cs AND "
				+ "EXISTS( SELECT csds FROM ClassroomSessionDivisionSubject csds WHERE csds.classroomSessionDivision = csd AND csds.teacher = :teacher )))))");
		*/
		registerNamedQuery(readByAcademicSessionByTeacher, "SELECT levelGroup FROM LevelGroup levelGroup WHERE EXISTS( "
				+ "SELECT csds FROM ClassroomSessionDivisionSubject csds WHERE csds.classroomSessionDivision.classroomSession.levelTimeDivision.level.group = levelGroup "
				+ "AND csds.teacher = :teacher AND csds.classroomSessionDivision.classroomSession.academicSession = :academicSession )");
	}
	
	@Override
	public Collection<LevelGroup> readByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher) {
		return namedQuery(readByAcademicSessionByTeacher).parameter(ClassroomSession.FIELD_ACADEMIC_SESSION, academicSession)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}

}
