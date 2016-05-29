package org.cyk.system.school.business.api.subject;

import java.util.Collection;

import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Evaluation;

public interface StudentClassroomSessionDivisionSubjectEvaluationBusiness extends TypedBusiness<StudentClassroomSessionDivisionSubjectEvaluation> {

	Collection<StudentClassroomSessionDivisionSubjectEvaluation> find(StudentClassroomSessionDivisionSubject studentSubject);
	
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject);
	
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);

	Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByStudentByClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision);
	
	Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByEvaluation(Evaluation evaluation,Boolean includeAll);
	
}
