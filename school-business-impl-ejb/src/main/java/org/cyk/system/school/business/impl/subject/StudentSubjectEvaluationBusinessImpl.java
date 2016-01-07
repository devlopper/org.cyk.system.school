package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.StudentSubjectEvaluationBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;

public class StudentSubjectEvaluationBusinessImpl extends AbstractTypedBusinessService<StudentSubjectEvaluation, StudentSubjectEvaluationDao> implements StudentSubjectEvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private StudentSubjectDao studentSubjectDao;
	
	@Inject
	public StudentSubjectEvaluationBusinessImpl(StudentSubjectEvaluationDao dao) {
		super(dao); 
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubjectEvaluation> find(StudentSubject studentSubject) {
		return dao.readByStudentSubject(studentSubject);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubjectEvaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubjectEvaluation> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubjectEvaluation> findByStudentByClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByStudentByClassroomSessionDivision(student,classroomSessionDivision);
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<StudentSubjectEvaluation> findBySubjectEvaluation(SubjectEvaluation subjectEvaluation,Boolean includeAll) {
		Collection<StudentSubjectEvaluation> collection = dao.readBySubjectEvaluation(subjectEvaluation);
		if(Boolean.TRUE.equals(includeAll)){
			for(StudentSubject studentSubject : studentSubjectDao.readByClassroomSessionDivisionSubject(subjectEvaluation
					.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject())){
				Boolean found = Boolean.FALSE;
				for(StudentSubjectEvaluation s : collection){
					if(s.getStudentSubject().getIdentifier().equals(studentSubject.getIdentifier())){
						found = Boolean.TRUE;
						break;
					}
				}
				if(Boolean.FALSE.equals(found)){
					StudentSubjectEvaluation studentSubjectEvaluation = new StudentSubjectEvaluation(subjectEvaluation, studentSubject, null);
					collection.add(studentSubjectEvaluation);
				}
			}
		}
		return collection;
	}

    
}
