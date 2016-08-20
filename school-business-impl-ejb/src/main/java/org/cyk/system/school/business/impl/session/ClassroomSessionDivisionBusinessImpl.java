package org.cyk.system.school.business.impl.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;

@Stateless
public class ClassroomSessionDivisionBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivision, ClassroomSessionDivisionDao> implements ClassroomSessionDivisionBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private ClassroomSessionDao classroomSessionDao;
	
	@Inject
	public ClassroomSessionDivisionBusinessImpl(ClassroomSessionDivisionDao dao) {
		super(dao);  
	}
	
	@Override
	public ClassroomSessionDivision create(ClassroomSessionDivision classroomSessionDivision) {
		classroomSessionDivision.setIndex(dao.countByClassroomSession(classroomSessionDivision.getClassroomSession()).byteValue());
		commonUtils.increment(Long.class, classroomSessionDivision.getClassroomSession(), ClassroomSession.FIELD_NUMBER_OF_DIVISIONS, 1l);
		classroomSessionDao.update(classroomSessionDivision.getClassroomSession());
		return super.create(classroomSessionDivision);
	}
	
	@Override
	public void computeResults(Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions) {
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			Collection<WeightedValue> weightedValues = new ArrayList<>();
			NodeResults results = classroomSessionDivision.getResults();
			results.setAverageLowest(BigDecimal.ZERO);
			results.setAverage(BigDecimal.ZERO);
			results.setAverageHighest(BigDecimal.ZERO);
			results.setNumberOfStudent(0);
			results.setNumberOfStudentPassingEvaluationAverage(0);
			for(StudentClassroomSessionDivision s : studentClassroomSessionDivisions){
				if(!s.getClassroomSessionDivision().equals(classroomSessionDivision) || s.getResults().getEvaluationSort().getAverage().getValue()==null)
					continue;
				
				s.setClassroomSessionDivision(classroomSessionDivision);
				
				weightedValues.add(new WeightedValue(s.getResults().getEvaluationSort().getAverage().getValue(),BigDecimal.ONE,Boolean.TRUE));
				if(s.getResults().getEvaluationSort().getAverage().getValue()==null)
					continue;
				results.setNumberOfStudent(results.getNumberOfStudent()+1);
				if(classroomSessionDivision.getResults().getAverageHighest()==null || s.getResults().getEvaluationSort().getAverage().getValue().compareTo(classroomSessionDivision.getResults().getAverageHighest())>0)
					classroomSessionDivision.getResults().setAverageHighest(s.getResults().getEvaluationSort().getAverage().getValue());
				if(classroomSessionDivision.getResults().getAverageLowest()==null || s.getResults().getEvaluationSort().getAverage().getValue().compareTo(classroomSessionDivision.getResults().getAverageLowest())<0)
					classroomSessionDivision.getResults().setAverageLowest(s.getResults().getEvaluationSort().getAverage().getValue());	
				
				//TODO should be take first on subject if null on higher
				if(s.getResults().getEvaluationSort().getAverage().getValue().compareTo(s.getClassroomSessionDivision().getClassroomSession().getAcademicSession()
						.getNodeInformations().getEvaluationPassAverage())>=0){
					results.setNumberOfStudentPassingEvaluationAverage(results.getNumberOfStudentPassingEvaluationAverage()+1);
				}
			}
			if(weightedValues.isEmpty()){
				
			}else{
				Average average = inject(MathematicsBusiness.class).average(weightedValues, null, null);
				classroomSessionDivision.getResults().setAverage(average.getValue());
			}
			dao.update(classroomSessionDivision);
		}
	}

	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession) {
		return findByClassroomSessions(Arrays.asList(classroomSession));
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return dao.readByClassroomSessions(classroomSessions);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSessionsByIndex(Collection<ClassroomSession> classroomSessions,Byte index) {
		return dao.readByClassroomSessionsByIndex(classroomSessions,index);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSessionDivision findByClassroomSessionByIndex(ClassroomSession classroomSession, Byte index) {
		return dao.readByClassroomSessionByIndex(classroomSession,index);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSessionByTeacher(ClassroomSession classroomSession, Teacher teacher) {
		return dao.readByClassroomSessionByTeacher(classroomSession,teacher);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByLevelTimeDivision(LevelTimeDivision levelTimeDivision) {
		return dao.readByLevelTimeDivision(levelTimeDivision); 
	}
	
}
