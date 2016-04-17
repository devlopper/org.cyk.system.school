package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;

public interface StudentClassroomSessionBusiness extends AbstractStudentResultsBusiness<ClassroomSession,StudentClassroomSession,StudentClassroomSessionDivision> {

	Collection<StudentClassroomSession> findByClassroomSession(ClassroomSession classroomSession);

	StudentClassroomSession finddByStudentByClassroomSession(Student student,ClassroomSession classroomSession);

	StudentClassroomSession findByStudentByClassroomSession(Student student, ClassroomSession classroomSession);
	
}
