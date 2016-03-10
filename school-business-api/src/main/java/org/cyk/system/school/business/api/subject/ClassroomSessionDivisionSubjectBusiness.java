package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;

public interface ClassroomSessionDivisionSubjectBusiness extends TypedBusiness<ClassroomSessionDivisionSubject> {

	void computeResults(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<StudentSubject> studentSubjects);
	Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision, Teacher teacher);
	
}
