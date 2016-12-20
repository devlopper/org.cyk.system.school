package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;

public interface ClassroomSessionDivisionSubjectBusiness extends TypedBusiness<ClassroomSessionDivisionSubject> {

	void computeResults(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<StudentClassroomSessionDivisionSubject> studentSubjects);
	Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision, Teacher teacher);
	ClassroomSessionDivisionSubject findByClassroomSessionDivisionBySubject(ClassroomSessionDivision classroomSessionDivision, Subject subject);
	
}
