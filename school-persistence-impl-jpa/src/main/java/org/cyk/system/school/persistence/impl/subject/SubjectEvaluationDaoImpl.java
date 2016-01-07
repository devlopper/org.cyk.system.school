package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationDao;

public class SubjectEvaluationDaoImpl extends AbstractTypedDao<SubjectEvaluation> implements SubjectEvaluationDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivisionSubject;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivisionSubject, _select().where(commonUtils.attributePath(SubjectEvaluation.FIELD_TYPE
				, ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT), ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT));
	}
	
	@Override
	public Collection<SubjectEvaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivisionSubject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_SUBJECT, classroomSessionDivisionSubject).resultMany();
	}
	
}
 