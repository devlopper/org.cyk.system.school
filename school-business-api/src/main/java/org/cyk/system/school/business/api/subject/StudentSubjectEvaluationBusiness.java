package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;

public interface StudentSubjectEvaluationBusiness extends TypedBusiness<StudentSubjectEvaluation> {

	Collection<StudentSubjectEvaluation> find(StudentSubject studentSubject);
	
	Collection<StudentSubjectEvaluation> findBySubject(ClassroomSessionDivisionSubject subject);
	
	Collection<StudentSubjectEvaluation> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	
	//Average average(Collection<EvaluatedStudent> evaluatedStudents);
	
}
