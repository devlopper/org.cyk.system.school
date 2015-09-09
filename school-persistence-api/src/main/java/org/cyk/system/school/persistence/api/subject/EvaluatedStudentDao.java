package org.cyk.system.school.persistence.api.subject;

import java.util.Collection;

import org.cyk.system.root.persistence.api.TypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;

public interface EvaluatedStudentDao extends TypedDao<EvaluatedStudent> {

	Collection<EvaluatedStudent> readByStudentSubject(StudentSubject studentSubject);
	Long countByStudentSubject(StudentSubject studentSubject);

	Collection<EvaluatedStudent> readBySubject(Subject subject);
	Collection<EvaluatedStudent> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<EvaluatedStudent> readByClassroomSession(ClassroomSession classroomSession);
	Collection<EvaluatedStudent> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	Collection<EvaluatedStudent> readBySubjects(Collection<Subject> levels);
	Collection<EvaluatedStudent> readByClassroomSessions(Collection<ClassroomSession> levels);
	
}
