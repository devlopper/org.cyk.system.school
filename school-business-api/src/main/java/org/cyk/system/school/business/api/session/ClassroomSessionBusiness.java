package org.cyk.system.school.business.api.session;

import java.math.BigDecimal;
import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;

public interface ClassroomSessionBusiness extends TypedBusiness<ClassroomSession> {

	Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession);
	Collection<ClassroomSession> findByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher);
	
	ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,String suffix);
	
	CommonNodeInformations findCommonNodeInformations(ClassroomSession classroomSession);
	
	BigDecimal convertAttendanceTimeToDivisionDuration(ClassroomSession classroomSession,Long millisecond);
	Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration);
	Collection<ClassroomSession> findByAcademicSessionByLevelGroup(AcademicSession academicSession,LevelGroup levelGroup);
	Collection<ClassroomSession> findByAcademicSessionByLevelGroupByTeacher(AcademicSession academicSession,LevelGroup levelGroup,Teacher teacher);

}
