package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.util.Collection;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.school.business.api.subject.EvaluatedStudentBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.EvaluatedStudent;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.EvaluatedStudentDao;

public class EvaluatedStudentBusinessImpl extends AbstractTypedBusinessService<EvaluatedStudent, EvaluatedStudentDao> implements EvaluatedStudentBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	//@Inject private MathematicsBusiness mathematicsBusiness;
	//@Inject private SchoolBusinessLayer schoolBusinessLayer;
	
	@Inject
	public EvaluatedStudentBusinessImpl(EvaluatedStudentDao dao) {
		super(dao); 
	}

	@Override
	public Collection<EvaluatedStudent> find(StudentSubject studentSubject) {
		return dao.readByStudentSubject(studentSubject);
	}
	
	@Override
	public Collection<EvaluatedStudent> findBySubject(Subject subject) {
		return dao.readBySubject(subject);
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
	public Collection<EvaluatedStudent> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}

	

    
}
