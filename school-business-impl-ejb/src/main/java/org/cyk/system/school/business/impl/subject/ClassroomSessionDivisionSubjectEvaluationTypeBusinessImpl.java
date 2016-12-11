package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;

public class ClassroomSessionDivisionSubjectEvaluationTypeBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubjectEvaluationType, ClassroomSessionDivisionSubjectEvaluationTypeDao> implements ClassroomSessionDivisionSubjectEvaluationTypeBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionDivisionSubjectEvaluationTypeBusinessImpl(ClassroomSessionDivisionSubjectEvaluationTypeDao dao) {
		super(dao); 
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name)){
			return new Object[]{classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject(),classroomSessionDivisionSubjectEvaluationType.getEvaluationType()};
		}
		return super.getPropertyValueTokens(classroomSessionDivisionSubjectEvaluationType, name);
	}
	
	@Override
	protected void beforeCreate(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		super.beforeCreate(classroomSessionDivisionSubjectEvaluationType);
		ClassroomSessionDivisionSubjectEvaluationType existing = dao.readByClassroomSessionDivisionSubjectByEvaluationType(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject()
				, classroomSessionDivisionSubjectEvaluationType.getEvaluationType());
		exceptionUtils().exception(existing!=null, "classroomSessionDivisionSubjectEvaluationType.alreadyexists");//TODO create shortcut method
		commonUtils.increment(Long.class, classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject(), ClassroomSessionDivisionSubject.FIELD_NUMBER_OF_EVALUATION_TYPES, 1l);
		inject(ClassroomSessionDivisionSubjectDao.class).update(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject());
	}
		
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSessionDivisionSubjectEvaluationType findByClassroomSessionDivisionSubjectByEvaluationType(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType) {
		return dao.readByClassroomSessionDivisionSubjectByEvaluationType(classroomSessionDivisionSubject, evaluationType);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionSubjectEvaluationType> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}

	
}
