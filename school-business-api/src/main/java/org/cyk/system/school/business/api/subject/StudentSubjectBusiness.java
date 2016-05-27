package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;

public interface StudentSubjectBusiness extends AbstractStudentResultsBusiness<ClassroomSessionDivisionSubject,StudentSubject, StudentSubjectEvaluation> {
	
	Collection<StudentSubject> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject);

	Collection<StudentSubject> findByStudent(Student student);
	StudentSubject findByStudentByClassroomSessionDivisionSubject(Student student, ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	Collection<StudentSubject> findByStudentByClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision,Teacher teacher);
	Collection<StudentSubject> findByStudentByClassroomSession(Student student, ClassroomSession classroomSession);
}
