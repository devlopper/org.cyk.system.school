package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;
import org.cyk.utility.common.computation.ArithmeticOperator;

public class StudentClassroomSessionDivisionSubjectEvaluationDaoImpl extends AbstractTypedDao<StudentClassroomSessionDivisionSubjectEvaluation> implements StudentClassroomSessionDivisionSubjectEvaluationDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByStudentSubject,countByStudentSubject,readByClassroomSessionDivisionSubject,readByClassroomSessionDivision,readByClassroomSession
    	,readByClassroomSessionDivisions,readBySubjects,readByClassroomSessions,readBySubjectEvaluation,readByStudentByClassroomSessionDivision;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentSubject, _select().where(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT));
        registerNamedQuery(readByClassroomSessionDivisionSubject, _select().where(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT,StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT),
        		StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT));
        
        registerNamedQuery(readByStudentByClassroomSessionDivision, _select().where(
        		commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT,StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION),ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		.and(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT,StudentClassroomSessionDivisionSubject.FIELD_STUDENT),StudentClassroomSessionDivisionSubject.FIELD_STUDENT,ArithmeticOperator.EQ));
        
        registerNamedQuery(readByClassroomSessionDivision, _select().where(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION), ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION));
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION) , ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)));
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT)));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)));
        registerNamedQuery(readBySubjectEvaluation, _select().where(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_EVALUATION));
    }
    
	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByStudentSubject(StudentClassroomSessionDivisionSubject studentSubject) {
		return namedQuery(readByStudentSubject).parameter(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, studentSubject)
                .resultMany();
	}
	
	@Override
	public Long countByStudentSubject(StudentClassroomSessionDivisionSubject studentSubject) {
		return countNamedQuery(countByStudentSubject).parameter(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT, studentSubject)
                .resultOne();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivisionSubject).parameter(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject)
                .resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessionDivisionSubjects(Collection<ClassroomSessionDivisionSubject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByEvaluation(Evaluation subjectEvaluation) {
		return namedQuery(readBySubjectEvaluation).parameter(StudentClassroomSessionDivisionSubjectEvaluation.FIELD_EVALUATION, subjectEvaluation).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByStudentByClassroomSessionDivision).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student)
				.parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision).resultMany();
	}
}
 