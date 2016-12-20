package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;

@Stateless
public class EvaluationBusinessImpl extends AbstractTypedBusinessService<Evaluation, EvaluationDao> implements EvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public EvaluationBusinessImpl(EvaluationDao dao) {
		super(dao); 
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Evaluation instanciateOne(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		Evaluation evaluation = new Evaluation();
		
		for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject)){
			evaluation.getStudentSubjectEvaluations().add(new StudentClassroomSessionDivisionSubjectEvaluation(evaluation, studentClassroomSessionDivisionSubject, null));
		}
		return evaluation;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Evaluation instanciateOne(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		exceptionUtils().cannotCreateMoreThan(dao.countByClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType)
				,classroomSessionDivisionSubjectEvaluationType.getCountInterval(),  Evaluation.class);
		Evaluation evaluation = super.instanciateOne();
		evaluation.setName(classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getName());
		return evaluation;
	}

	@Override
	public Evaluation create(Evaluation evaluation) {
		logIdentifiable("Creating evaluation", evaluation);
		Long numberOfEvaluations = dao.countByClassroomSessionDivisionSubjectEvaluationType(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		logTrace("Number of evaluations found : {}", numberOfEvaluations);
		exceptionUtils().cannotCreateMoreThan(numberOfEvaluations,evaluation.getClassroomSessionDivisionSubjectEvaluationType().getCountInterval(),  Evaluation.class);
		commonUtils.increment(Long.class, evaluation.getClassroomSessionDivisionSubjectEvaluationType(), ClassroomSessionDivisionSubjectEvaluationType.FIELD_NUMBER_OF_EVALUATIONS, 1l);
		inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).update(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		evaluation = super.create(evaluation);
		save(evaluation);
		return evaluation;
	}
	
	@Override
	public Evaluation save(Evaluation evaluation,Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations) {
		evaluation = dao.update(evaluation);
		evaluation.setStudentSubjectEvaluations(studentSubjectEvaluations);
		
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> database = inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class)
				.readByEvaluation(evaluation);
		
		delete(StudentClassroomSessionDivisionSubjectEvaluation.class,database, evaluation.getStudentSubjectEvaluations());
		
		save(evaluation);
		return evaluation;
	}
	
	private void save(Evaluation evaluation){
		for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : evaluation.getStudentSubjectEvaluations()){
			studentSubjectEvaluation.setEvaluation(evaluation);
			if(studentSubjectEvaluation.getIdentifier()==null)
				inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).create(studentSubjectEvaluation);
			else
				inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).update(studentSubjectEvaluation);		
		}
	}
	
	@Override
	public Evaluation delete(Evaluation evaluation) {
		for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).readByEvaluation(evaluation))
			inject(StudentClassroomSessionDivisionSubjectEvaluationDao.class).delete(studentSubjectEvaluation);
		
		commonUtils.increment(Long.class, evaluation.getClassroomSessionDivisionSubjectEvaluationType(), ClassroomSessionDivisionSubjectEvaluationType.FIELD_NUMBER_OF_EVALUATIONS, -1l);
		inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).update(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		
		return super.delete(evaluation);
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
