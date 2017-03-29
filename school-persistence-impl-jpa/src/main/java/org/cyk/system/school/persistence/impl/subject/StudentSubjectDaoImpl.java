package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.computation.ArithmeticOperator;
import org.cyk.utility.common.computation.LogicalOperator;

public class StudentSubjectDaoImpl extends AbstractTypedDao<StudentClassroomSessionDivisionSubject> /*AbstractStudentResultsDaoImpl<ClassroomSessionDivisionSubject,StudentSubject,StudentSubjectEvaluation>*/ implements StudentClassroomSessionDivisionSubjectDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByStudentBySubject,readBySubject,readByStudent,readByClassroomSessionDivision,readByClassroomSession,readByClassroomSessionDivisions
    	,readBySubjects,readByClassroomSessions,readByStudentByClassroomSessionDivision,readByStudentByClassroomSessionDivisionBySubject,readByStudentByClassroomSession
    	,readByClassroomSessionDivisionByTeacher,readWhereStudentResultsEvaluationAverageIsNullByClassroomSessionDivisionOrderNumber;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentBySubject, _select().where(StudentClassroomSessionDivisionSubject.FIELD_STUDENT).where(LogicalOperator.AND,StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
        		,StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT,ArithmeticOperator.EQ));
        registerNamedQuery(readBySubject, _select().where(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT));
        registerNamedQuery(readByStudent, _select().where(StudentClassroomSessionDivisionSubject.FIELD_STUDENT));
        registerNamedQuery(readByClassroomSessionDivision, _select().where(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION));
        registerNamedQuery(readByStudentByClassroomSessionDivision, _select()
        		.where(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION),ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		.and(StudentClassroomSessionDivisionSubject.FIELD_STUDENT));
        registerNamedQuery(readByStudentByClassroomSessionDivisionBySubject, _select()
        		.where(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION),ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		.and(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_SUBJECT),ClassroomSessionDivisionSubject.FIELD_SUBJECT,ArithmeticOperator.EQ)
        		.and(StudentClassroomSessionDivisionSubject.FIELD_STUDENT));
        
        registerNamedQuery(readByStudentByClassroomSession, _select()
        		.where(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION),ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		.and(StudentClassroomSessionDivisionSubject.FIELD_STUDENT));
        
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)
        		,ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		));
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
        		, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)));
        
        registerNamedQuery(readByClassroomSessionDivisionByTeacher, _select().where(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION)
        		,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION).and(commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.FIELD_TEACHER)
                		,ClassroomSessionDivisionSubject.FIELD_TEACHER, ArithmeticOperator.EQ));
        
        
        registerNamedQuery(readWhereStudentResultsEvaluationAverageIsNullByClassroomSessionDivisionOrderNumber, "SELECT r FROM StudentClassroomSessionDivisionSubject r "
        		+ " WHERE r.results.evaluationSort.average.value IS NULL "
        		+ "AND r.classroomSessionDivisionSubject.classroomSessionDivision.globalIdentifier.orderNumber = :orderNumber"
        		);
        
        registerNamedQuery(readDuplicates, "SELECT r FROM StudentClassroomSessionDivisionSubject r "
        		+ " GROUP BY r.classroomSessionDivisionSubject,r.student "
        		+ " HAVING COUNT(r) > 1 "
        		+ " ORDER BY "
        		+ "   r.student.globalIdentifier.code "
        		+ " , r.classroomSessionDivisionSubject.classroomSessionDivision.classroomSession.levelTimeDivision.level.levelName.globalIdentifier.code "
        		+ " , r.classroomSessionDivisionSubject.classroomSessionDivision.globalIdentifier.orderNumber"
        		+ " , r.classroomSessionDivisionSubject.subject.globalIdentifier.code"
        		
        		);
        
        registerNamedQuery(countDuplicates, "SELECT COUNT(r) FROM StudentClassroomSessionDivisionSubject r "
        		+ " GROUP BY r.classroomSessionDivisionSubject , r.student "
        		+ " HAVING COUNT(r) > 1"
        		);
    }
    /*
    @Override
    protected String getLevelFieldName() {
    	return StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT;
    }*/
    
    @Override
    public StudentClassroomSessionDivisionSubject readByStudentBySubject(Student student,ClassroomSessionDivisionSubject subject) {
        return namedQuery(readByStudentBySubject).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student).parameter(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, subject).ignoreThrowable(NoResultException.class)
                .resultOne();
    }

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject subject) {
		return namedQuery(readBySubject).parameter(StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, subject).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}

	
	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByStudent(Student student) {
		return namedQuery(readByStudent).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByStudentByClassroomSessionDivision).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student)
				.parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision).resultMany();
	}
	
	@Override
	public StudentClassroomSessionDivisionSubject readByStudentByClassroomSessionDivisionBySubject(Student student, ClassroomSessionDivision classroomSessionDivision,Subject subject) {
		return namedQuery(readByStudentByClassroomSessionDivisionBySubject).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student)
				.parameter(ClassroomSessionDivisionSubject.FIELD_SUBJECT, subject)
				.parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision).ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByStudentByClassroomSession(Student student, ClassroomSession classroomSession) {
		return namedQuery(readByStudentByClassroomSession).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student)
				.parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession).resultMany();
	}
	
	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision, Teacher teacher) {
		return namedQuery(readByClassroomSessionDivisionByTeacher).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision)
				.parameter(ClassroomSessionDivisionSubject.FIELD_TEACHER, teacher).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readDuplicatesByStudentByClassroomSessionDivisionBySubject(Student student, ClassroomSessionDivision classroomSessionDivision, Subject subject) {
		return namedQuery(readByStudentByClassroomSessionDivisionBySubject).parameter(StudentClassroomSessionDivisionSubject.FIELD_STUDENT, student)
			.parameter(ClassroomSessionDivisionSubject.FIELD_SUBJECT, subject)
			.parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision).resultMany();
	}

	@Override
	public Collection<StudentClassroomSessionDivisionSubject> readWhereStudentResultsEvaluationAverageIsNullByClassroomSessionDivisionOrderNumber(
			Long classroomSessionDivisionOrderNumber) {
		return namedQuery(readWhereStudentResultsEvaluationAverageIsNullByClassroomSessionDivisionOrderNumber)
				.parameter(GlobalIdentifier.FIELD_ORDER_NUMBER, classroomSessionDivisionOrderNumber)
				.resultMany();
	}
}
 