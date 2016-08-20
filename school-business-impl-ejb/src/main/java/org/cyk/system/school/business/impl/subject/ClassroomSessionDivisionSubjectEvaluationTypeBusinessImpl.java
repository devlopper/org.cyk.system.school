package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;

@Stateless
public class ClassroomSessionDivisionSubjectEvaluationTypeBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubjectEvaluationType, ClassroomSessionDivisionSubjectEvaluationTypeDao> implements ClassroomSessionDivisionSubjectEvaluationTypeBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private ClassroomSessionDivisionSubjectDao classroomSessionDivisionSubjectDao;
	
	@Inject
	public ClassroomSessionDivisionSubjectEvaluationTypeBusinessImpl(ClassroomSessionDivisionSubjectEvaluationTypeDao dao) {
		super(dao); 
	}
	
	@Override
	public ClassroomSessionDivisionSubjectEvaluationType create(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		if(classroomSessionDivisionSubjectEvaluationType.getCountInterval()!=null)
			inject(IntervalBusiness.class).create(classroomSessionDivisionSubjectEvaluationType.getCountInterval());
		commonUtils.increment(Long.class, classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject(), ClassroomSessionDivisionSubject.FIELD_NUMBER_OF_EVALUATION_TYPES, 1l);
		classroomSessionDivisionSubjectDao.update(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject());
		return super.create(classroomSessionDivisionSubjectEvaluationType);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSessionDivisionSubjectEvaluationType findBySubjectByEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType evaluationTypeName) {
		return dao.readBySubjectByEvaluationType(subject, evaluationTypeName);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionSubjectEvaluationType> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}

	
}
