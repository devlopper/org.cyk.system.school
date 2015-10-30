package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.StudentSubjectEvaluationBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.StudentSubjectEvaluationDao;

public class StudentSubjectEvaluationBusinessImpl extends AbstractTypedBusinessService<StudentSubjectEvaluation, StudentSubjectEvaluationDao> implements StudentSubjectEvaluationBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public StudentSubjectEvaluationBusinessImpl(StudentSubjectEvaluationDao dao) {
		super(dao); 
	}

	@Override
	public Collection<StudentSubjectEvaluation> find(StudentSubject studentSubject) {
		return dao.readByStudentSubject(studentSubject);
	}
	
	@Override
	public Collection<StudentSubjectEvaluation> findByClassroomSessionDivisionSubject(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		return dao.readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
	}
	/*
	@Override
	public Average average(Collection<EvaluatedStudent> evaluatedStudents) {
		Average average = new Average();
		for(EvaluatedStudent evaluatedStudent : evaluatedStudents)
			average.getWeightables().add(new EvaluatedStudentWeight(evaluatedStudent));
		mathematicsBusiness.average(average, schoolBusinessLayer.getAverageComputationListener(), schoolBusinessLayer.getAverageComputationScript());
		return average;
	}
	*/

	@Override
	public Collection<StudentSubjectEvaluation> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}

	

    
}
