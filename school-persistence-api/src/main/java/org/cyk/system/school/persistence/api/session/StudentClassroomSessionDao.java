package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession.SearchCriteria;

public interface StudentClassroomSessionDao extends TypedDao<StudentClassroomSession> {

	Collection<StudentClassroomSession> readByClassroomSession(ClassroomSession classroomSession);
	Long countByClassroomSession(ClassroomSession classroomSession);
	
	StudentClassroomSession readByStudentByClassroomSession(Student student, ClassroomSession classroomSession);

	Collection<StudentClassroomSession> readByClassroomSessions(Collection<ClassroomSession> levels);

	Collection<StudentClassroomSession> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision);

	Collection<StudentClassroomSession> readByCriteria(SearchCriteria criteria);
	Long countByCriteria(SearchCriteria criteria);

	Collection<StudentClassroomSession> readByAcademicSession(AcademicSession academicSession);
}
