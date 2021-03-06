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
import org.cyk.system.school.model.session.ClassroomSession.SearchCriteria;
import org.cyk.system.school.model.session.ClassroomSessionSuffix;

public interface ClassroomSessionBusiness extends TypedBusiness<ClassroomSession> {

	Collection<ClassroomSession> findByAcademicSession(AcademicSession academicSession);
	Collection<ClassroomSession> findByAcademicSessionByTeacher(AcademicSession academicSession,Teacher teacher);
	Collection<ClassroomSession> findByAcademicSessionByCoordinator(AcademicSession academicSession, Teacher coordinator);
	
	Collection<ClassroomSession> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision);
	
	ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,String suffix);
	ClassroomSession findByAcademicSessionByLevelTimeDivisionBySuffix(AcademicSession academicSession,LevelTimeDivision levelTimeDivision, ClassroomSessionSuffix suffix);
	ClassroomSession findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(String levelTimeDivisionCode,String suffixCode);
	
	CommonNodeInformations findCommonNodeInformations(ClassroomSession classroomSession);
	Collection<FileRepresentationType> findStudentClassroomSessionDivisionResultsFileRepresentationTypes(Collection<ClassroomSession> classroomSessions);
	Collection<ClassroomSession> findByStudentClassroomSessionDivisions(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivision);
	
	BigDecimal convertAttendanceTimeToDivisionDuration(ClassroomSession classroomSession,Long millisecond);
	Long convertAttendanceTimeToMillisecond(ClassroomSession classroomSession,BigDecimal duration);
	Collection<ClassroomSession> findByAcademicSessionByLevelGroup(AcademicSession academicSession,LevelGroup levelGroup);
	Collection<ClassroomSession> findByAcademicSessionByLevelGroupByTeacher(AcademicSession academicSession,LevelGroup levelGroup,Teacher teacher);
	Collection<ClassroomSession> findByLevelNameBySuffix(String levelNameCode,String suffix);
	Collection<ClassroomSession> findByLevelName(String levelNameCode);
	
	void computeResults(Collection<ClassroomSession> classroomSessions,Collection<StudentClassroomSession> studentClassroomSessions);
	
	Collection<ClassroomSession> findByCriteria(SearchCriteria searchCriteria);
	Long countByCriteria(SearchCriteria searchCriteria);
	
	ClassroomSession instanciateOne(String levelTimeDivisionCode, String suffixCode,String coordinatorCode, String timeDivisionTypeCode,
			String[][] divisions, String[][] subjects, String[][] evaluationTypes, String[][] metricCollections);
	
}
