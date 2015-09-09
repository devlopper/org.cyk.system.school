package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;

public interface EvaluatedStudentBusiness extends TypedBusiness<EvaluatedStudent> {

	Collection<EvaluatedStudent> find(StudentSubject studentSubject);
	
	Collection<EvaluatedStudent> findBySubject(Subject subject);
	
	Collection<EvaluatedStudent> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	
	//Average average(Collection<EvaluatedStudent> evaluatedStudents);
	
}
