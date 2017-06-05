package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;

public interface ClassroomSessionDivisionBusiness extends TypedBusiness<ClassroomSessionDivision> {

	// should be moved to a super interface
	void computeResults(Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
	//Collection<MetricCollection> findMetricCollectionsByMetricCollectionTypes(Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<MetricCollectionType> metricCollectionTypes);
	
	Collection<ClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession);
	Collection<ClassroomSessionDivision> findByClassroomSessions(Collection<ClassroomSession> classroomSessions);
	Collection<ClassroomSessionDivision> findByClassroomSessionsByOrderNumber(Collection<ClassroomSession> classroomSessions,Long orderNumber);
	Collection<ClassroomSessionDivision> findByClassroomSessionByTeacher(ClassroomSession classroomSession,Teacher teacher);

	ClassroomSessionDivision findByClassroomSessionByOrderNumber(ClassroomSession classroomSession,Long orderNumber);

	Collection<ClassroomSessionDivision> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision);
	Collection<ClassroomSessionDivision> findByLevelNameByClassroomSessionSuffixByClassroomSessionDivisionOrderNumber(String levelNameCode,String classroomSessionSuffixCode,Long classroomSessionDivisionOrderNumber);
	Collection<ClassroomSessionDivision> findByLevelNameByClassroomSessionDivisionOrderNumber(String levelNameCode,Long classroomSessionDivisionOrderNumber);
	
	Collection<ClassroomSessionDivision> findByLevelTimeDivisionCodeByClassroomSessionSuffixCodeByClassroomSessionDivisionOrderNumber(String levelTimeDivisionCode,String classroomSessionSuffixCode,Long classroomSessionDivisionOrderNumber);
	Collection<ClassroomSessionDivision> findByLevelTimeDivisionCodeByClassroomSessionDivisionOrderNumber(String levelTimeDivisionCode,Long classroomSessionDivisionOrderNumber);

	ClassroomSessionDivision instanciateOne(ClassroomSession classroomSession,Long orderNumber);
	
}
