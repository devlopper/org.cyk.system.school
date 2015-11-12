package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;

public interface ClassroomSessionDivisionSubjectBusiness extends TypedBusiness<ClassroomSessionDivisionSubject> {

	void computeResults(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<StudentSubject> studentSubjects);
	
}
