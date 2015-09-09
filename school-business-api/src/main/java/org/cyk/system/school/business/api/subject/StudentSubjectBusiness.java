package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;

public interface StudentSubjectBusiness extends AbstractStudentResultsBusiness<Subject,StudentSubject, EvaluatedStudent> {
	
	Collection<StudentSubject> findBySubject(Subject subject);

	StudentSubject findByStudentBySubject(Student student, Subject subject);
	
}
