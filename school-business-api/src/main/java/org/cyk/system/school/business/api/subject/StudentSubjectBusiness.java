package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface StudentSubjectBusiness extends AbstractStudentResultsBusiness<ClassroomSessionDivisionSubject,StudentSubject, StudentSubjectEvaluation> {
	
	Collection<StudentSubject> findBySubject(ClassroomSessionDivisionSubject subject);

	StudentSubject findByStudentBySubject(Student student, ClassroomSessionDivisionSubject subject);
	
}
