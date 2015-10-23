package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;

public class StudentSubjectEvaluationDaoImpl extends AbstractTypedDao<StudentSubjectEvaluation> implements StudentSubjectEvaluationDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;
	
    private String readByStudentSubject,countByStudentSubject,readBySubject,readByClassroomSessionDivision,readByClassroomSession
    	,readByClassroomSessionDivisions,readBySubjects,readByClassroomSessions,readBySubjectEvaluation;
    
    @Override
    protected void namedQueriesInitialisation() {
        super.namedQueriesInitialisation();
        registerNamedQuery(readByStudentSubject, _select().where(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT));
        registerNamedQuery(readBySubject, _select().where(commonUtils.attributePath(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT,StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT),
        		StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT));
        registerNamedQuery(readByClassroomSessionDivision, _select().where(commonUtils.attributePath(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION), ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION));
        registerNamedQuery(readByClassroomSession, _select().where(commonUtils.attributePath(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION) , ClassroomSessionDivision.FIELD_CLASSROOMSESSION));
        registerNamedQuery(readByClassroomSessionDivisions, _select().whereIdentifierIn(commonUtils.attributePath(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION)));
        registerNamedQuery(readBySubjects, _select().whereIdentifierIn(commonUtils.attributePath(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT)));
        registerNamedQuery(readByClassroomSessions, _select().whereIdentifierIn(commonUtils.attributePath(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, StudentSubject.FIELD_CLASSROOMSESSIONDIVISIONSUBJECT,ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION,ClassroomSessionDivision.FIELD_CLASSROOMSESSION)));
        registerNamedQuery(readBySubjectEvaluation, _select().where(StudentSubjectEvaluation.FIELD_EVALUATION));
    }
    
	@Override
	public Collection<StudentSubjectEvaluation> readByStudentSubject(StudentSubject studentSubject) {
		return namedQuery(readByStudentSubject).parameter(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, studentSubject)
                .resultMany();
	}
	
	@Override
	public Long countByStudentSubject(StudentSubject studentSubject) {
		return countNamedQuery(countByStudentSubject).parameter(StudentSubjectEvaluation.FIELD_STUDENT_SUBJECT, studentSubject)
                .resultOne();
	}

	@Override
	public Collection<StudentSubjectEvaluation> readBySubject(ClassroomSessionDivisionSubject subject) {
		return namedQuery(readBySubject).parameter(ClassroomSessionDivisionSubject.FIELD_SUBJECT, subject)
                .resultMany();
	}

	@Override
	public Collection<StudentSubjectEvaluation> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOMSESSIONDIVISION, classroomSessionDivision)
                .resultMany();
	}

	@Override
	public Collection<StudentSubjectEvaluation> readByClassroomSession(ClassroomSession classroomSession) {
		return namedQuery(readByClassroomSession).parameter(ClassroomSessionDivision.FIELD_CLASSROOMSESSION, classroomSession)
                .resultMany();
	}
	
	@Override
	public Collection<StudentSubjectEvaluation> readByClassroomSessionDivisions(Collection<ClassroomSessionDivision> classroomSessionDivisions) {
		return namedQuery(readByClassroomSessionDivisions).parameterIdentifiers(classroomSessionDivisions).resultMany();
	}

	@Override
	public Collection<StudentSubjectEvaluation> readBySubjects(Collection<ClassroomSessionDivisionSubject> subjects) {
		return namedQuery(readBySubjects).parameterIdentifiers(subjects).resultMany();
	}

	@Override
	public Collection<StudentSubjectEvaluation> readByClassroomSessions(Collection<ClassroomSession> levels) {
		return namedQuery(readByClassroomSessions).parameterIdentifiers(levels).resultMany();
	}

	@Override
	public Collection<StudentSubjectEvaluation> readBySubjectEvaluation(SubjectEvaluation subjectEvaluation) {
		return namedQuery(readBySubjectEvaluation).parameter(StudentSubjectEvaluation.FIELD_EVALUATION, subjectEvaluation).resultMany();
	}
}
 