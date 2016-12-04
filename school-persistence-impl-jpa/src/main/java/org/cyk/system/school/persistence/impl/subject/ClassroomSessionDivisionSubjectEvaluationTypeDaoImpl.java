package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;

public class ClassroomSessionDivisionSubjectEvaluationTypeDaoImpl extends AbstractTypedDao<ClassroomSessionDivisionSubjectEvaluationType> implements ClassroomSessionDivisionSubjectEvaluationTypeDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivisionSubjectByEvaluationType,readByClassroomSessionDivisionSubject;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivisionSubjectByEvaluationType, _select().where(ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT).and(ClassroomSessionDivisionSubjectEvaluationType.FIELD_TYPE));
		registerNamedQuery(readByClassroomSessionDivisionSubject, _select().where(ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT));
	}
	
	@Override
	public ClassroomSessionDivisionSubjectEvaluationType readByClassroomSessionDivisionSubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName) {
		return namedQuery(readByClassroomSessionDivisionSubjectByEvaluationType).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT, subject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_TYPE, evaluationTypeName)
				.ignoreThrowable(NoResultException.class).resultOne();
	}

	@Override
	public Collection<ClassroomSessionDivisionSubjectEvaluationType> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivisionSubject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT, classroomSessionDivisionSubject).resultMany();
	}
	
}
 