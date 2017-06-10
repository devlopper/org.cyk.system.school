package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;

public interface ClassroomSessionDivisionSubjectDao extends TypedDao<ClassroomSessionDivisionSubject> {

	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<ClassroomSessionDivisionSubject> readWhereStudentExistByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	
	Collection<ClassroomSessionDivisionSubject> readByClassroomSession(ClassroomSession classroomSession);
	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionBySubject(ClassroomSession classroomSession,Subject subject);
	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	Collection<ClassroomSessionDivisionSubject> readByClassroomSessions(Collection<ClassroomSession> levels);
	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision, Teacher teacher);
	ClassroomSessionDivisionSubject readByClassroomSessionDivisionBySubject(ClassroomSessionDivision classroomSessionDivision, Subject subject);
	
	Collection<ClassroomSessionDivisionSubject> readManyByClassroomSessionDivisionBySubject(ClassroomSessionDivision classroomSessionDivision, Subject subject);
	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisionOrderNumber(Long classroomSessionDivisionOrderNumber);
}
