package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;

public interface StudentClassroomSessionSubjectBusiness extends AbstractStudentResultsBusiness<ClassroomSessionSubject,StudentClassroomSessionSubject, StudentClassroomSessionDivisionSubject> {
	
	StudentClassroomSessionSubject instanciateOne(StudentClassroomSession studentClassroomSession,ClassroomSessionSubject classroomSessionSubject);
	Collection<StudentClassroomSessionSubject> findByStudentClassroomSession(StudentClassroomSession studentClassroomSession);
}
