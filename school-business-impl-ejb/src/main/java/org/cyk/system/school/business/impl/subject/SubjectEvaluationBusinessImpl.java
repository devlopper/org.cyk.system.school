package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationDao;

@Stateless
public class SubjectEvaluationBusinessImpl extends AbstractTypedBusinessService<SubjectEvaluation, SubjectEvaluationDao> implements SubjectEvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentSubjectEvaluationDao studentSubjectEvaluationDao;
	
	@Inject
	public SubjectEvaluationBusinessImpl(SubjectEvaluationDao dao) {
		super(dao); 
	}

	@Override
	public SubjectEvaluation create(SubjectEvaluation subjectEvaluation) {
		if(subjectEvaluation.getDate()==null)
			subjectEvaluation.setDate(universalTimeCoordinated());
		subjectEvaluation = super.create(subjectEvaluation);
		for(StudentSubjectEvaluation studentSubjectEvaluation : subjectEvaluation.getStudentSubjectEvaluations()){
			studentSubjectEvaluation.setSubjectEvaluation(subjectEvaluation);
			exceptionUtils().exception(studentSubjectEvaluation.getStudentSubject()==null, "no student set");
			exceptionUtils().exception(studentSubjectEvaluation.getValue()==null, "no note set");
			studentSubjectEvaluationDao.create(studentSubjectEvaluation);
		}
		return subjectEvaluation;
	}
	
}
