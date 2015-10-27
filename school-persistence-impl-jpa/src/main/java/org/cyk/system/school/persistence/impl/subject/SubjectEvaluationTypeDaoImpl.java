package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;

import javax.persistence.NoResultException;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationTypeDao;

public class SubjectEvaluationTypeDaoImpl extends AbstractTypedDao<SubjectEvaluationType> implements SubjectEvaluationTypeDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readBySubjectByEvaluationType;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readBySubjectByEvaluationType, _select().where(SubjectEvaluationType.FIELD_SUBJECT).and(SubjectEvaluationType.FIELD_TYPE));
	}
	
	@Override
	public SubjectEvaluationType readBySubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName) {
		return namedQuery(readBySubjectByEvaluationType).parameter(SubjectEvaluationType.FIELD_SUBJECT, subject).parameter(SubjectEvaluationType.FIELD_TYPE, evaluationTypeName)
				.ignoreThrowable(NoResultException.class).resultOne();
	}
	
}
 