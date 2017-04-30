package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;

public class ClassroomSessionDivisionSubjectEvaluationTypeDaoImpl extends AbstractTypedDao<ClassroomSessionDivisionSubjectEvaluationType> implements ClassroomSessionDivisionSubjectEvaluationTypeDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivisionSubjectByEvaluationType,readByClassroomSessionDivisionSubjects,readByClassroomSessionDivision;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivisionSubjectByEvaluationType, _select().where(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT).and(ClassroomSessionDivisionSubjectEvaluationType.FIELD_EVALUATION_TYPE));
		registerNamedQuery(readByClassroomSessionDivisionSubjects, _select().whereIdentifierIn(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT));
		registerNamedQuery(readByClassroomSessionDivision, _select().where(commonUtils.attributePath(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, ClassroomSessionDivisionSubject.COLUMN_CLASSROOM_SESSION_DIVISION)
				,ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION));
	}
	
	@Override
	public ClassroomSessionDivisionSubjectEvaluationType readByClassroomSessionDivisionSubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName) {
		return namedQuery(readByClassroomSessionDivisionSubjectByEvaluationType).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, subject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_EVALUATION_TYPE, evaluationTypeName)
				.ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivisionSubjects(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects) {
		return namedQuery(readByClassroomSessionDivisionSubjects).parameterIdentifiers(classroomSessionDivisionSubjects).resultMany();
	}
	
	@Override
	public Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return readByClassroomSessionDivisionSubjects(Arrays.asList(classroomSessionDivisionSubject));
	}
	
	@Override
	public Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return namedQuery(readByClassroomSessionDivision).parameter(ClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION, classroomSessionDivision).resultMany();
	}
	
}
 