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
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;

@Stateless
public class EvaluationBusinessImpl extends AbstractTypedBusinessService<Evaluation, EvaluationDao> implements EvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentSubjectEvaluationDao studentSubjectEvaluationDao;
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeDao classroomSessionDivisionSubjectEvaluationTypeDao;
	
	@Inject
	public EvaluationBusinessImpl(EvaluationDao dao) {
		super(dao); 
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Evaluation newInstance(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		Evaluation evaluation = new Evaluation();
		evaluation.setDate(universalTimeCoordinated());
		for(StudentSubject studentSubject : studentSubjectDao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject)){
			evaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(evaluation, studentSubject, null));
		}
		return evaluation;
	}

	@Override
	public Evaluation create(Evaluation evaluation) {
		logIdentifiable("Creating evaluation", evaluation);
		Long numberOfEvaluations = dao.countByClassroomSessionDivisionSubjectEvaluationType(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		logTrace("Number of evaluations found : {}", numberOfEvaluations);
		exceptionUtils().cannotCreateMoreThan(numberOfEvaluations,evaluation.getClassroomSessionDivisionSubjectEvaluationType().getCountInterval(),  Evaluation.class);
		if(evaluation.getDate()==null)
			evaluation.setDate(universalTimeCoordinated());
		commonUtils.increment(Long.class, evaluation.getClassroomSessionDivisionSubjectEvaluationType(), ClassroomSessionDivisionSubjectEvaluationType.FIELD_NUMBER_OF_EVALUATIONS, 1l);
		classroomSessionDivisionSubjectEvaluationTypeDao.update(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		evaluation = super.create(evaluation);
		save(evaluation);
		return evaluation;
	}
	
	@Override
	public Evaluation save(Evaluation evaluation,Collection<StudentSubjectEvaluation> studentSubjectEvaluations) {
		evaluation = dao.update(evaluation);
		evaluation.setStudentSubjectEvaluations(studentSubjectEvaluations);
		
		Collection<StudentSubjectEvaluation> database = studentSubjectEvaluationDao.readByEvaluation(evaluation);
		
		delete(StudentSubjectEvaluation.class,studentSubjectEvaluationDao,database, evaluation.getStudentSubjectEvaluations());
		
		save(evaluation);
		return evaluation;
	}
	
	private void save(Evaluation evaluation){
		for(StudentSubjectEvaluation studentSubjectEvaluation : evaluation.getStudentSubjectEvaluations()){
			studentSubjectEvaluation.setEvaluation(evaluation);
			if(studentSubjectEvaluation.getIdentifier()==null)
				studentSubjectEvaluationDao.create(studentSubjectEvaluation);
			else
				studentSubjectEvaluationDao.update(studentSubjectEvaluation);		
		}
	}
	
	@Override
	public Evaluation delete(Evaluation evaluation) {
		for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluationDao.readByEvaluation(evaluation))
			studentSubjectEvaluationDao.delete(studentSubjectEvaluation);
		
		commonUtils.increment(Long.class, evaluation.getClassroomSessionDivisionSubjectEvaluationType(), ClassroomSessionDivisionSubjectEvaluationType.FIELD_NUMBER_OF_EVALUATIONS, -1l);
		classroomSessionDivisionSubjectEvaluationTypeDao.update(evaluation.getClassroomSessionDivisionSubjectEvaluationType());
		
		return super.delete(evaluation);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<Evaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}
}
