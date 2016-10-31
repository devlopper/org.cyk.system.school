package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.SubjectClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;

@Stateless
public class ClassroomSessionDivisionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubject, ClassroomSessionDivisionSubjectDao> implements ClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject private SubjectClassroomSessionDao subjectClassroomSessionDao;
	@Inject private ClassroomSessionDivisionDao classroomSessionDivisionDao;
	
	@Inject
	public ClassroomSessionDivisionSubjectBusinessImpl(ClassroomSessionDivisionSubjectDao dao) {
		super(dao); 
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSessionDivisionSubject classroomSessionDivisionSubject, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{classroomSessionDivisionSubject.getClassroomSessionDivision(),classroomSessionDivisionSubject.getSubject()};
		return super.getPropertyValueTokens(classroomSessionDivisionSubject, name);
	}
	
	@Override
	public ClassroomSessionDivisionSubject create(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.create(classroomSessionDivisionSubject);
		SubjectClassroomSession subjectClassroomSession = subjectClassroomSessionDao.readBySubjectByClassroomSession(
				classroomSessionDivisionSubject.getSubject(),classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession());
		if(subjectClassroomSession==null){
			subjectClassroomSession = new SubjectClassroomSession(classroomSessionDivisionSubject.getSubject(),classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession());
			subjectClassroomSessionDao.create(subjectClassroomSession);
		}
		commonUtils.increment(Long.class, classroomSessionDivisionSubject.getClassroomSessionDivision(), ClassroomSessionDivision.FIELD_NUMBER_OF_SUBJECTS, 1l);
		classroomSessionDivisionDao.update(classroomSessionDivisionSubject.getClassroomSessionDivision());
		
		for(Student student : inject(StudentDao.class).readByClassroomSessionDivision(classroomSessionDivisionSubject.getClassroomSessionDivision())){
			StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(student,classroomSessionDivisionSubject);
			studentClassroomSessionDivisionSubject.setCascadeOperationToChildren(Boolean.FALSE);
			studentClassroomSessionDivisionSubject.setCascadeOperationToMaster(Boolean.TRUE);
			inject(StudentClassroomSessionDivisionSubjectBusiness.class).create(studentClassroomSessionDivisionSubject);
		}
		
		return classroomSessionDivisionSubject;
	}
	
	private void cascade(ClassroomSessionDivisionSubject classroomSessionDivisionSubject){
		
	}
	
	@Override
	public ClassroomSessionDivisionSubject delete(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		cascade(classroomSessionDivisionSubject);
		for(StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject : inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject))
			inject(StudentClassroomSessionDivisionSubjectBusiness.class).delete(studentClassroomSessionDivisionSubject);
		return super.delete(classroomSessionDivisionSubject);
	}
    
	//TODO should be factored in higher class
	@Override
	public void computeResults(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<StudentClassroomSessionDivisionSubject> studentSubjects) {
		logTrace("Computing node results of {} Classroom Session Division Subject(s). Number of students={}", classroomSessionDivisionSubjects.size(),studentSubjects.size());
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			NodeResults results = classroomSessionDivisionSubject.getResults();
			results.setAverageLowest(BigDecimal.ZERO);
			results.setAverage(BigDecimal.ZERO);
			results.setAverageHighest(BigDecimal.ZERO);
			results.setNumberOfStudentPassingEvaluationAverage(0);
			Collection<WeightedValue> weightedValues = new ArrayList<>();
			for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
				if(studentSubject.getClassroomSessionDivisionSubject().getIdentifier().equals(classroomSessionDivisionSubject.getIdentifier())){
					BigDecimal value = studentSubject.getResults().getEvaluationSort().getAverage().getValue();
					if(value==null){
						
					}else{
						weightedValues.add(new WeightedValue(value,BigDecimal.ONE));
						if(value.compareTo(results.getAverageLowest())==-1){
							results.setAverageLowest(value);
						}
						if(value.compareTo(results.getAverageHighest())==1){
							results.setAverageHighest(value);
						}
						//TODO should be take first on subject if null on higher
						if(value.compareTo(studentSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession().getAcademicSession()
								.getNodeInformations().getEvaluationPassAverage())>=0){
							results.setNumberOfStudentPassingEvaluationAverage(results.getNumberOfStudentPassingEvaluationAverage()+1);
						}
					}
				}else{
					
				}
			}
			results.setNumberOfStudent(weightedValues.size());
			results.setAverage(inject(MathematicsBusiness.class).average(weightedValues, null, null).getValue());
			logIdentifiable("Average computed", classroomSessionDivisionSubject);
			dao.update(classroomSessionDivisionSubject);
		}
	}

	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision) {
		return dao.readByClassroomSessionDivision(classroomSessionDivision);
	}
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public Collection<ClassroomSessionDivisionSubject> findByClassroomSessionDivisionByTeacher(ClassroomSessionDivision classroomSessionDivision,Teacher teacher) {
		return dao.readByClassroomSessionDivisionByTeacher(classroomSessionDivision,teacher);
	}
	
	/**/
	
	public static interface Listener extends AbstractIdentifiableBusinessServiceImpl.Listener<ClassroomSessionDivisionSubject> {
		
		/**/
		
		Collection<String> getEvaluationTypeCodesToCreate();
		Listener addEvaluationTypeCodesToCreate(String...codes);
		Boolean getAutoCreateEvaluationTypeCodesOnCreate();
		Listener setAutoCreateEvaluationTypeCodesOnCreate(Boolean value);
		
		@Getter @Setter
		public static class Adapter extends AbstractIdentifiableBusinessServiceImpl.Listener.Adapter.Default<ClassroomSessionDivisionSubject> implements Listener {
			private static final long serialVersionUID = 1L;

			protected Collection<String> evaluationTypeCodesToCreate;
			protected Boolean autoCreateEvaluationTypeCodesOnCreate;
			
			@Override
			public Listener addEvaluationTypeCodesToCreate(String...codes) {
				return null;
			}
			
			@Override
			public Listener setAutoCreateEvaluationTypeCodesOnCreate(Boolean value) {
				return null;
			}
			
			/**/
			
			public static class Default extends Listener.Adapter implements Serializable {

				private static final long serialVersionUID = 1L;

				/**/
				
				public static class EnterpriseResourcePlanning extends Default implements Serializable {

					private static final long serialVersionUID = 1L;

					/**/
					
					
				}
				
			}
			
		}
	}

}
