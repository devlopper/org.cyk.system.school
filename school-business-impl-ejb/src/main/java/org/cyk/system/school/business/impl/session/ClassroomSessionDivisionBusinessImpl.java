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

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
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
	
	@Inject
	public ClassroomSessionDivisionBusinessImpl(ClassroomSessionDivisionDao dao) {
		super(dao);  
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSessionDivision classroomSessionDivision, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{classroomSessionDivision.getClassroomSession(),classroomSessionDivision.getTimeDivisionType(),classroomSessionDivision.getOrderNumber()};
		return super.getPropertyValueTokens(classroomSessionDivision, name);
	}
	
	@Override
	protected void beforeCreate(ClassroomSessionDivision classroomSessionDivision) {
		super.beforeCreate(classroomSessionDivision);
		Long start = classroomSessionDivision.getClassroomSession().getAcademicSession().getNodeInformations().getClassroomSessionDivisionOrderNumberInterval()==null ?
				0 : inject(IntervalBusiness.class).findGreatestLowestValue(classroomSessionDivision.getClassroomSession().getAcademicSession().getNodeInformations()
						.getClassroomSessionDivisionOrderNumberInterval()).longValue();
		classroomSessionDivision.setOrderNumber(start+dao.countByClassroomSession(classroomSessionDivision.getClassroomSession()));
		commonUtils.increment(Long.class, classroomSessionDivision.getClassroomSession(), ClassroomSession.FIELD_NUMBER_OF_DIVISIONS, 1l);
		inject(ClassroomSessionDao.class).update(classroomSessionDivision.getClassroomSession());
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
	/*
	@Override @TransactionAttribute(TransactionAttributeType.SUPPORTS)
	public Collection<MetricCollection> findMetricCollectionsByMetricCollectionTypes(Collection<ClassroomSessionDivision> classroomSessionDivisions
			,Collection<org.cyk.system.root.model.mathematics.MetricCollectionType> metricCollectionTypes) {
		Collection<MetricCollectionIdentifiableGlobalIdentifier> metricCollectionIdentifiableGlobalIdentifiers 
			= inject(MetricCollectionIdentifiableGlobalIdentifierBusiness.class).findByTypesByIdentifiables(metricCollectionTypes, classroomSessionDivisions);
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions)
			for(org.cyk.system.root.model.mathematics.MetricCollectionType metricCollectionType : metricCollectionTypes)
				if
		return null;
	}
	*/
	
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSession(ClassroomSession classroomSession) {
		return findByClassroomSessions(Arrays.asList(classroomSession));
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSessions(Collection<ClassroomSession> classroomSessions) {
		return dao.readByClassroomSessions(classroomSessions);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivision> findByClassroomSessionsByOrderNumber(Collection<ClassroomSession> classroomSessions,Long orderNumber) {
		return dao.readByClassroomSessionsByOrderNumber(classroomSessions,orderNumber);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSessionDivision findByClassroomSessionByOrderNumber(ClassroomSession classroomSession, Long orderNumber) {
		return dao.readByClassroomSessionByOrderNumber(classroomSession,orderNumber);
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
