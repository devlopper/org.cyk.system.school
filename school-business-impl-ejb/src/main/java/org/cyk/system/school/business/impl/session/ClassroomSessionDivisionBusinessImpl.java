package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;

public class ClassroomSessionDivisionBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivision, ClassroomSessionDivisionDao> implements ClassroomSessionDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private MathematicsBusiness mathematicsBusiness;
	
	@Inject private ClassroomSessionDivisionSubjectDao classroomSessionDivisionSubjectDao;
	
	@Inject
	public ClassroomSessionDivisionBusinessImpl(ClassroomSessionDivisionDao dao) {
		super(dao);  
	}
	
	@Override
	public void results(Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			//Collection<StudentClassroomSessionDivision> lStudentClassroomSessionDivisions = new ArrayList<>();
			Collection<WeightedValue> weightedValues = new ArrayList<>();
			Integer numberOfStudent = 0;
			for(StudentClassroomSessionDivision s : studentClassroomSessionDivisions){
				if(!s.getClassroomSessionDivision().equals(classroomSessionDivision))
					continue;
				
				s.setClassroomSessionDivision(classroomSessionDivision);
				
				weightedValues.add(new WeightedValue(s.getResults().getEvaluationSort().getAverage().getValue(),BigDecimal.ONE,Boolean.TRUE));
				if(s.getResults().getEvaluationSort().getAverage().getValue()==null)
					continue;
				numberOfStudent++;
				if(classroomSessionDivision.getResults().getAverageHighest()==null || s.getResults().getEvaluationSort().getAverage().getValue().compareTo(classroomSessionDivision.getResults().getAverageHighest())>0)
					classroomSessionDivision.getResults().setAverageHighest(s.getResults().getEvaluationSort().getAverage().getValue());
				if(classroomSessionDivision.getResults().getAverageLowest()==null || s.getResults().getEvaluationSort().getAverage().getValue().compareTo(classroomSessionDivision.getResults().getAverageLowest())<0)
					classroomSessionDivision.getResults().setAverageLowest(s.getResults().getEvaluationSort().getAverage().getValue());	
			}
			Average average = mathematicsBusiness.average(weightedValues, null, null);
			classroomSessionDivision.getResults().setAverage(average.getValue());
			classroomSessionDivision.getResults().setNumberOfStudent(numberOfStudent);
		}
	}

	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<ClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession) {
		return dao.readByClassroomSession(classroomSession);
	}

	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Integer findIndex(ClassroomSessionDivision classroomSessionDivision) {
		Integer index = 0;
		for(ClassroomSessionDivision c : dao.readByClassroomSession(classroomSessionDivision.getClassroomSession())){
			index++;
			if(classroomSessionDivision.equals(c))
				break;
		}
		return index;
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public String format(ClassroomSessionDivision classroomSessionDivision) {
		return classroomSessionDivision.getTimeDivisionType().getUiString()+" "+findIndex(classroomSessionDivision);
	}
	
	@Override
	protected void __load__(ClassroomSessionDivision classroomSessionDivision) {
		super.__load__(classroomSessionDivision);
		classroomSessionDivision.setSubjects(classroomSessionDivisionSubjectDao.readByClassroomSessionDivision(classroomSessionDivision));
	}

	
	

}
