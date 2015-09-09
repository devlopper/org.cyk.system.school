package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.persistence.api.subject.EvaluatedStudentDao;
import org.cyk.system.school.persistence.api.subject.EvaluationDao;

@Stateless
public class EvaluationBusinessImpl extends AbstractTypedBusinessService<Evaluation, EvaluationDao> implements EvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private EvaluatedStudentDao evaluatedStudentDao;
	
	@Inject
	public EvaluationBusinessImpl(EvaluationDao dao) {
		super(dao); 
	}

	@Override
	public Evaluation create(Evaluation evaluation) {
		evaluation = super.create(evaluation);
		for(EvaluatedStudent evaluatedStudent : evaluation.getEvaluatedStudents()){
			evaluatedStudent.setEvaluation(evaluation);
			exceptionUtils().exception(evaluatedStudent.getStudentSubject()==null, "no student set");
			exceptionUtils().exception(evaluatedStudent.getValue()==null, "no note set");
			evaluatedStudentDao.create(evaluatedStudent);
		}
		return evaluation;
	}
	
}
