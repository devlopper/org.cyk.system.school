package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;

public interface StudentClassroomSessionDivisionSubjectBusiness extends AbstractStudentResultsBusiness<ClassroomSessionDivisionSubject,StudentClassroomSessionDivisionSubject, StudentClassroomSessionDivisionSubjectEvaluation> {
	
	Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject);

	Collection<StudentClassroomSessionDivisionSubject> findByStudent(Student student);
	StudentClassroomSessionDivisionSubject findByStudentByClassroomSessionDivisionSubject(Student student, ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	Collection<StudentClassroomSessionDivisionSubject> findByStudentByClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<StudentClassroomSessionDivisionSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision,Teacher teacher);
	Collection<StudentClassroomSessionDivisionSubject> findByStudentByClassroomSession(Student student, ClassroomSession classroomSession);
}
