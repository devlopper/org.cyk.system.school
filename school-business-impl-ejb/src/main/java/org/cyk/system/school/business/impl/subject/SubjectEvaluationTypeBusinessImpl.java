package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.SubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationTypeDao;

@Stateless
public class SubjectEvaluationTypeBusinessImpl extends AbstractTypedBusinessService<SubjectEvaluationType, SubjectEvaluationTypeDao> implements SubjectEvaluationTypeBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public SubjectEvaluationTypeBusinessImpl(SubjectEvaluationTypeDao dao) {
		super(dao); 
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public SubjectEvaluationType findBySubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName) {
		return dao.readBySubjectByEvaluationType(subject, evaluationTypeName);
	}

	
}
