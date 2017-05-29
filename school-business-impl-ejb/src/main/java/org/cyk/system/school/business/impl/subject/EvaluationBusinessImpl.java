package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;

public class EvaluationBusinessImpl extends AbstractTypedBusinessService<Evaluation, EvaluationDao> implements EvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public EvaluationBusinessImpl(EvaluationDao dao) {
		super(dao); 
	}
	
	@Override
	protected Object[] getPropertyValueTokens(Evaluation evaluation, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{evaluation.getClassroomSessionDivisionSubjectEvaluationType(),evaluation.getClassroomSessionDivisionSubjectEvaluationType().getNumberOfEvaluations()};
		return super.getPropertyValueTokens(evaluation, name);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Evaluation instanciateOne(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		Evaluation evaluation = new Evaluation();
		
		for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject)){
			evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().addOne(new StudentClassroomSessionDivisionSubjectEvaluation(evaluation, studentClassroomSessionDivisionSubject, null));
		}
		return evaluation;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Evaluation instanciateOne(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		exceptionUtils().cannotCreateMoreThan(dao.countByClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType)
				,classroomSessionDivisionSubjectEvaluationType.getCountInterval(),  Evaluation.class);
		Evaluation evaluation = instanciateOne(classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject());
		evaluation.setClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType);
		return evaluation;
	}
	
	@Override
	public Evaluation instanciateOne(String classroomSessionDivisionSubjectEvaluationTypeCode,String[][] values) {
		ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = read(ClassroomSessionDivisionSubjectEvaluationType.class, classroomSessionDivisionSubjectEvaluationTypeCode);
		Evaluation evaluation = instanciateOne(classroomSessionDivisionSubjectEvaluationType);
		evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection().clear();
		for(String[] value : values){
			evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().addOne(new StudentClassroomSessionDivisionSubjectEvaluation(evaluation
					, read(StudentClassroomSessionDivisionSubject.class,value[0]), commonUtils.getBigDecimal(value[1])));
		}
		return evaluation;
	}
	
	@Override
	protected void beforeCreate(Evaluation evaluation) {
		super.beforeCreate(evaluation);
		Long numberOfEvaluations = dao.countByClassroomSessionDivisionSubjectEvaluationType(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		logTrace("Number of evaluations found : {}", numberOfEvaluations);
		exceptionUtils().cannotCreateMoreThan(numberOfEvaluations,evaluation.getClassroomSessionDivisionSubjectEvaluationType().getCountInterval(),  Evaluation.class);
	}
	
	@Override
	protected void afterCreate(Evaluation evaluation) {
		super.afterCreate(evaluation);
		commonUtils.increment(Long.class, evaluation.getClassroomSessionDivisionSubjectEvaluationType(), ClassroomSessionDivisionSubjectEvaluationType.FIELD_NUMBER_OF_EVALUATIONS, 1l);
		inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).update(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		
		if(evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().isSynchonizationEnabled()){
			inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).create(evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection());
		}
	}
	
	@Override
	protected void afterUpdate(Evaluation evaluation) {
		super.afterUpdate(evaluation);
		if(evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().isSynchonizationEnabled()){
			Collection<StudentClassroomSessionDivisionSubjectEvaluation> database = inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class)
					.readByEvaluation(evaluation);
			delete(StudentClassroomSessionDivisionSubjectEvaluation.class,database, evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection());	
			inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).save(evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection());
		}
	}
	
	@Override
	protected void beforeDelete(Evaluation evaluation) {
		super.beforeDelete(evaluation);
		for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).readByEvaluation(evaluation))
			inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).delete(studentSubjectEvaluation);
		
		commonUtils.increment(Long.class, evaluation.getClassroomSessionDivisionSubjectEvaluationType(), ClassroomSessionDivisionSubjectEvaluationType.FIELD_NUMBER_OF_EVALUATIONS, -1l);
		inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).update(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
	}
		
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<Evaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<Evaluation> findByClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		return dao.readByClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType);
	}
}
