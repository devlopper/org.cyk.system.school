package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectEvaluationDao;

public class StudentClassroomSessionDivisionSubjectEvaluationBusinessImpl extends AbstractTypedBusinessService<StudentClassroomSessionDivisionSubjectEvaluation, StudentClassroomSessionDivisionSubjectEvaluationDao> implements StudentClassroomSessionDivisionSubjectEvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentClassroomSessionDivisionSubjectDao studentClassroomSessionDivisionSubjectDao;
	
	@Inject
	public StudentClassroomSessionDivisionSubjectEvaluationBusinessImpl(StudentClassroomSessionDivisionSubjectEvaluationDao dao) {
		super(dao); 
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> find(StudentClassroomSessionDivisionSubject studentSubject) {
		return dao.readByStudentSubject(studentSubject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByStudentByClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByStudentByClassroomSessionDivision(student,classroomSessionDivision);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByEvaluation(Evaluation evaluation,Boolean includeAll) {
		Collection<StudentClassroomSessionDivisionSubjectEvaluation> collection = dao.readByEvaluation(evaluation);
		if(Boolean.TRUE.equals(includeAll)){
			for(StudentClassroomSessionDivisionSubject studentSubject : studentClassroomSessionDivisionSubjectDao.readByClassroomSessionDivisionSubject(evaluation
					.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject())){
				Boolean found = Boolean.FALSE;
				for(StudentClassroomSessionDivisionSubjectEvaluation s : collection){
					if(s.getStudentClassroomSessionDivisionSubject().getIdentifier().equals(studentSubject.getIdentifier())){
						found = Boolean.TRUE;
						break;
					}
				}
				if(Boolean.FALSE.equals(found)){
					StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation = new StudentClassroomSessionDivisionSubjectEvaluation(evaluation, studentSubject, null);
					collection.add(studentSubjectEvaluation);
				}
			}
		}
		return collection;
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentClassroomSessionDivisionSubjectEvaluation> findByEvaluation(Evaluation evaluation) {
		return findByEvaluation(evaluation, Boolean.FALSE);
	}
}
