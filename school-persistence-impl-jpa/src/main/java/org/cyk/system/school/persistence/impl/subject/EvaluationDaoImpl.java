package org.cyk.system.school.persistence.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.persistence.impl.AbstractTypedDao;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;

public class EvaluationDaoImpl extends AbstractTypedDao<Evaluation> implements EvaluationDao,Serializable {

	private static final long serialVersionUID = 6306356272165070761L;

	private String readByClassroomSessionDivisionSubject,countByClassroomSessionDivisionSubject,
	readByClassroomSessionDivisionSubjectEvaluationType,countByClassroomSessionDivisionSubjectEvaluationType;
	
	@Override
	protected void namedQueriesInitialisation() {
		super.namedQueriesInitialisation();
		registerNamedQuery(readByClassroomSessionDivisionSubject, _select().where(commonUtils.attributePath(Evaluation.FIELD_TYPE
				, ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT), ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT));
		registerNamedQuery(readByClassroomSessionDivisionSubjectEvaluationType, _select().where(Evaluation.FIELD_TYPE));
	}
	
	@Override
	public Collection<Evaluation> readByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return namedQuery(readByClassroomSessionDivisionSubject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject).resultMany();
	}
	
	@Override
	public Long countByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return countNamedQuery(countByClassroomSessionDivisionSubject).parameter(ClassroomSessionDivisionSubjectEvaluationType.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, classroomSessionDivisionSubject).resultOne();
	}

	@Override
	public Collection<Evaluation> readByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		return namedQuery(readByClassroomSessionDivisionSubjectEvaluationType).parameter(Evaluation.FIELD_TYPE, classroomSessionDivisionSubjectEvaluationType).resultMany();
	}
	
	@Override
	public Long countByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		return countNamedQuery(countByClassroomSessionDivisionSubjectEvaluationType).parameter(Evaluation.FIELD_TYPE, classroomSessionDivisionSubjectEvaluationType).resultOne();
	}
	
	
	
}
 