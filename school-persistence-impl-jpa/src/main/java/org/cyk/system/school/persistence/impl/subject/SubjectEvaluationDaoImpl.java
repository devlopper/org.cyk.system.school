package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationDao;

public class SubjectEvaluationDaoImpl extends AbstractTypedDao<SubjectEvaluation> implements SubjectEvaluationDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivisionSubject;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivisionSubject, _select().where(commonUtils.attributePath(SubjectEvaluation.FIELD_TYPE
				, SubjectEvaluationType.FIELD_SUBJECT), SubjectEvaluationType.FIELD_SUBJECT));
	}
	
	@Override
	public Collection<SubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivisionSubject).parameter(SubjectEvaluationType.FIELD_SUBJECT, classroomSessionDivisionSubject).resultMany();
	}
	
}
 