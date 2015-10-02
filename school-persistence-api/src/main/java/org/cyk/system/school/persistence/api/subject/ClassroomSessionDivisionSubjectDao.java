package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface ClassroomSessionDivisionSubjectDao extends TypedDao<ClassroomSessionDivisionSubject> {

	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<ClassroomSessionDivisionSubject> readByClassroomSession(ClassroomSession classroomSession);

	Collection<ClassroomSessionDivisionSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);

	Collection<ClassroomSessionDivisionSubject> readByClassroomSessions(Collection<ClassroomSession> levels);
	
}
