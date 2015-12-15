package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;

public interface StudentSubjectEvaluationBusiness extends TypedBusiness<StudentSubjectEvaluation> {

	Collection<StudentSubjectEvaluation> find(StudentSubject studentSubject);
	
	Collection<StudentSubjectEvaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
	Collection<StudentSubjectEvaluation> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentSubjectEvaluation> findByStudentByClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision);
	
	Collection<StudentSubjectEvaluation> findBySubjectEvaluation(SubjectEvaluation subjectEvaluation,Boolean includeAll);
	
}
