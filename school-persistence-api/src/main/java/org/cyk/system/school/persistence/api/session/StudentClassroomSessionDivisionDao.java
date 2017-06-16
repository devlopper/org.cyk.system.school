package org.cyk.system.school.persistence.api.session;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;

public interface StudentClassroomSessionDivisionDao extends TypedDao<StudentClassroomSessionDivision> {

	Collection<StudentClassroomSessionDivision> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	StudentClassroomSessionDivision readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentClassroomSessionDivision> readByClassroomSession(ClassroomSession classroomSession);

	Collection<StudentClassroomSessionDivision> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);

	Collection<StudentClassroomSessionDivision> readByClassroomSessions(Collection<ClassroomSession> levels);

	Collection<StudentClassroomSessionDivision> readByStudentByClassroomSession(Student student, ClassroomSession classroomSession);

	Collection<StudentClassroomSessionDivision> readByClassroomSessionDivisionIndex(Long classroomSessionDivisionOrderNumber);
	Long countByClassroomSessionDivisionIndex(Long classroomSessionDivisionOrderNumber);
	
	Collection<StudentClassroomSessionDivision> readByClassroomSessionByTeacher(ClassroomSession classroomSession,Teacher teacher);

	Collection<StudentClassroomSessionDivision> readByLevelTimeDivision(LevelTimeDivision levelTimeDivision);

	Collection<StudentClassroomSessionDivision> readByAcademicSession(AcademicSession academicSession);

	Collection<StudentClassroomSessionDivision> readByAcademicSessionByClassroomSessionDivisionOrderNumber(AcademicSession academicSession,Long classroomSessionDivisionOrderNumber);
}
