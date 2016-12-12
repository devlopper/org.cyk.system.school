package org.cyk.system.school.business.api.session;

import java.math.BigDecimal;
import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.model.file.FileRepresentationType;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;

public interface ClassroomSessionBusiness extends TypedBusiness<ClassroomSession> {

	Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession);
	Collection<ClassroomSession> findByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher);
	
	Collection<ClassroomSession> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision);
	
	ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,String suffix);
	
	CommonNodeInformations findCommonNodeInformations(ClassroomSession classroomSession);
	Collection<FileRepresentationType> findStudentClassroomSessionDivisionResultsFileRepresentationTypes(Collection<ClassroomSession> classroomSessions);
	Collection<ClassroomSession> findByStudentClassroomSessionDivisions(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivision);
	
	BigDecimal convertAttendanceTimeToDivisionDuration(ClassroomSession classroomSession,Long millisecond);
	Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration);
	Collection<ClassroomSession> findByAcademicSessionByLevelGroup(AcademicSession academicSession,LevelGroup levelGroup);
	Collection<ClassroomSession> findByAcademicSessionByLevelGroupByTeacher(AcademicSession academicSession,LevelGroup levelGroup,Teacher teacher);
	Collection<ClassroomSession> findByLevelNameBySuffix(String levelNameCode,String suffix);
	
	void computeResults(Collection<ClassroomSession> classroomSessions,Collection<StudentClassroomSession> studentClassroomSessions);
}
