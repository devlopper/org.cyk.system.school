package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;
import org.cyk.system.school.persistence.api.subject.SubjectEvaluationDao;

@Stateless
public class SubjectEvaluationBusinessImpl extends AbstractTypedBusinessService<SubjectEvaluation, SubjectEvaluationDao> implements SubjectEvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentSubjectEvaluationDao studentSubjectEvaluationDao;
	@Inject private StudentSubjectDao studentSubjectDao;
	
	@Inject
	public SubjectEvaluationBusinessImpl(SubjectEvaluationDao dao) {
		super(dao); 
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public SubjectEvaluation newInstance(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		SubjectEvaluation subjectEvaluation = new SubjectEvaluation();
		subjectEvaluation.setDate(universalTimeCoordinated());
		for(StudentSubject studentSubject : studentSubjectDao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject)){
			subjectEvaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(subjectEvaluation, studentSubject, null));
		}
		return subjectEvaluation;
	}

	@Override
	public SubjectEvaluation create(SubjectEvaluation subjectEvaluation) {
		if(subjectEvaluation.getDate()==null)
			subjectEvaluation.setDate(universalTimeCoordinated());
		subjectEvaluation = super.create(subjectEvaluation);
		save(subjectEvaluation);
		return subjectEvaluation;
	}
	
	@Override
	public SubjectEvaluation save(SubjectEvaluation subjectEvaluation,Collection<StudentSubjectEvaluation> studentSubjectEvaluations) {
		subjectEvaluation = dao.update(subjectEvaluation);
		subjectEvaluation.setStudentSubjectEvaluations(studentSubjectEvaluations);
		
		Collection<StudentSubjectEvaluation> database = studentSubjectEvaluationDao.readBySubjectEvaluation(subjectEvaluation);
		
		delete(StudentSubjectEvaluation.class,studentSubjectEvaluationDao,database, subjectEvaluation.getStudentSubjectEvaluations());
		
		save(subjectEvaluation);
		return subjectEvaluation;
	}
	
	private void save(SubjectEvaluation subjectEvaluation){
		for(StudentSubjectEvaluation studentSubjectEvaluation : subjectEvaluation.getStudentSubjectEvaluations()){
			studentSubjectEvaluation.setSubjectEvaluation(subjectEvaluation);
			exceptionUtils().exception(studentSubjectEvaluation.getStudentSubject()==null, "no student set");
			exceptionUtils().exception(studentSubjectEvaluation.getValue()==null, "no note set");
			if(studentSubjectEvaluation.getIdentifier()==null)
				studentSubjectEvaluationDao.create(studentSubjectEvaluation);
			else
				studentSubjectEvaluationDao.update(studentSubjectEvaluation);		
		}
	}
	
	@Override
	protected void __load__(SubjectEvaluation subjectEvaluation) {
		super.__load__(subjectEvaluation);
		subjectEvaluation.setStudentSubjectEvaluations(studentSubjectEvaluationDao.readBySubjectEvaluation(subjectEvaluation));
	}
	
}
