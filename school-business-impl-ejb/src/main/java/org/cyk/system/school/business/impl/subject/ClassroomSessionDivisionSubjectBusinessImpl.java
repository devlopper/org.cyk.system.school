package org.cyk.system.school.business.impl.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.inject.Inject;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.WeightedValue;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.AbstractTypedBusinessService;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.school.business.api.session.ClassroomSessionSubjectBusiness;
import org.cyk.system.school.business.api.session.CommonNodeInformationsBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.cyk.utility.common.Constant;

import lombok.Getter;
import lombok.Setter;

@Stateless
public class ClassroomSessionDivisionSubjectBusinessImpl extends AbstractTypedBusinessService<ClassroomSessionDivisionSubject, ClassroomSessionDivisionSubjectDao> implements ClassroomSessionDivisionSubjectBusiness,Serializable {

	private static final long serialVersionUID = -3799482462496328200L;
	
	@Inject
	public ClassroomSessionDivisionSubjectBusinessImpl(ClassroomSessionDivisionSubjectDao dao) {
		super(dao); 
	}
	
	@Override
	protected Collection<? extends org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl.Listener<?>> getListeners() {
		return Listener.COLLECTION;
	}
	
	@Override
	protected Object[] getPropertyValueTokens(ClassroomSessionDivisionSubject classroomSessionDivisionSubject, String name) {
		if(ArrayUtils.contains(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}, name))
			return new Object[]{classroomSessionDivisionSubject.getClassroomSessionDivision(),classroomSessionDivisionSubject.getSubject()};
		return super.getPropertyValueTokens(classroomSessionDivisionSubject, name);
	}
	
	@Override
	protected void beforeCreate(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.beforeCreate(classroomSessionDivisionSubject);
		//TODO make it as helper method
		if(classroomSessionDivisionSubject.getRequired()==null && classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
				.getLevelTimeDivision().getLevel().getLevelName().getRequired()!=null)
			classroomSessionDivisionSubject.setRequired(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
					.getLevelTimeDivision().getLevel().getLevelName().getRequired());
	}
		
	@Override
	protected void afterCreate(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.afterCreate(classroomSessionDivisionSubject);
		ClassroomSessionSubject classroomSessionSubject = inject(ClassroomSessionSubjectDao.class).readByClassroomSessionBySubject(
				classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession(),classroomSessionDivisionSubject.getSubject());
		if(classroomSessionSubject==null){
			classroomSessionSubject = new ClassroomSessionSubject(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
					,classroomSessionDivisionSubject.getSubject());
			inject(ClassroomSessionSubjectBusiness.class).create(classroomSessionSubject);
		}
		commonUtils.increment(Long.class, classroomSessionDivisionSubject.getClassroomSessionDivision(), ClassroomSessionDivision.FIELD_NUMBER_OF_SUBJECTS, 1l);
		inject(ClassroomSessionDivisionDao.class).update(classroomSessionDivisionSubject.getClassroomSessionDivision());
		
		if(classroomSessionDivisionSubject.getEvaluationTypes().isSynchonizationEnabled()){
			inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).create(classroomSessionDivisionSubject.getEvaluationTypes().getCollection());
		}
		
		if(Boolean.TRUE.equals(classroomSessionDivisionSubject.getRequired())){
			for(Student student : inject(StudentDao.class).readByClassroomSessionDivision(classroomSessionDivisionSubject.getClassroomSessionDivision())){
				StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = new StudentClassroomSessionDivisionSubject(student,classroomSessionDivisionSubject);
				studentClassroomSessionDivisionSubject.setCascadeOperationToChildren(Boolean.FALSE);
				studentClassroomSessionDivisionSubject.setCascadeOperationToMaster(Boolean.TRUE);
				inject(StudentClassroomSessionDivisionSubjectBusiness.class).create(studentClassroomSessionDivisionSubject);
			}
		}
	}
	
	private void cascade(ClassroomSessionDivisionSubject classroomSessionDivisionSubject){
		
	}
	
	@Override
	protected void beforeDelete(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super.beforeDelete(classroomSessionDivisionSubject);
		cascade(classroomSessionDivisionSubject);
		inject(StudentClassroomSessionDivisionSubjectBusiness.class).delete(inject(StudentClassroomSessionDivisionSubjectDao.class).readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
		inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).delete(inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).readByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
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
						BigDecimal pass = classroomSessionDivisionSubject.getEvaluationPassAverage();
						if(pass==null)
							pass = inject(CommonNodeInformationsBusiness.class).findValue(classroomSessionDivisionSubject.getClassroomSessionDivision().getClassroomSession()
									,BigDecimal.class,CommonNodeInformations.FIELD_EVALUATION_PASS_AVERAGE);
						if(value.compareTo(pass)>=0){
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
	
	@Override @TransactionAttribute(TransactionAttributeType.NEVER)
	public ClassroomSessionDivisionSubject findByClassroomSessionDivisionBySubject(ClassroomSessionDivision classroomSessionDivision,Subject subject) {
		return dao.readByClassroomSessionDivisionBySubject(classroomSessionDivision,subject);
	}
		
	@Override
	public ClassroomSessionDivisionSubject instanciateOne(String[] values) {
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = instanciateOne();
		Integer index = 0;
		String value;
		if(StringUtils.isNotBlank(value = values[index++]))
			classroomSessionDivisionSubject.setClassroomSessionDivision(inject(ClassroomSessionDivisionDao.class).read(value));
		if(StringUtils.isNotBlank(value = values[index++]))
			classroomSessionDivisionSubject.setSubject(inject(SubjectDao.class).read(value));
		if(StringUtils.isNotBlank(value = values[index++]))
			classroomSessionDivisionSubject.setTeacher(inject(TeacherDao.class).read(value));
		classroomSessionDivisionSubject.getEvaluationTypes().setSynchonizationEnabled(Boolean.TRUE);
		if(StringUtils.isNotBlank(value = values[index++]))
			for(String evaluationTypeInfos : StringUtils.split(value,Constant.CHARACTER_VERTICAL_BAR.toString())){
				String[] array = StringUtils.split(evaluationTypeInfos,Constant.CHARACTER_COMA.toString());
				ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class)
						.instanciateOne(new String[]{null,commonUtils.getValueAt(array, 0),commonUtils.getValueAt(array, 1),commonUtils.getValueAt(array, 2)
								,commonUtils.getValueAt(array, 3),commonUtils.getValueAt(array, 4)});
				classroomSessionDivisionSubjectEvaluationType.setClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
				classroomSessionDivisionSubject.getEvaluationTypes().getCollection().add(classroomSessionDivisionSubjectEvaluationType);
			}
		/*
		if(StringUtils.isNotBlank(value = values[index++]))
			classroomSessionDivisionSubject.setGroup(inject(ClassroomSessionDivisionSubjectGroupDao.class).read(value));
		*/
		return classroomSessionDivisionSubject;
	}
	
	@Override
	public ClassroomSessionDivisionSubject instanciateOne(ClassroomSessionDivision classroomSessionDivision,Subject subject) {
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = instanciateOne();
		classroomSessionDivisionSubject.setClassroomSessionDivision(classroomSessionDivision);
		classroomSessionDivisionSubject.setSubject(subject);
		return classroomSessionDivisionSubject;
	}
	
	/**/
	
	public static interface Listener extends AbstractIdentifiableBusinessServiceImpl.Listener<ClassroomSessionDivisionSubject> {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		Collection<ClassroomSessionDivisionSubjectEvaluationType> getClassroomSessionDivisionSubjectEvaluationTypeModels();
		Listener addClassroomSessionDivisionSubjectEvaluationTypeModel(ClassroomSessionDivisionSubjectEvaluationType...models);
		Boolean getUseClassroomSessionDivisionSubjectEvaluationTypeModelsOnCreate();
		Listener setUseClassroomSessionDivisionSubjectEvaluationTypeModelsOnCreate(Boolean value);
		
		@Getter @Setter
		public static class Adapter extends AbstractIdentifiableBusinessServiceImpl.Listener.Adapter.Default<ClassroomSessionDivisionSubject> implements Listener {
			private static final long serialVersionUID = 1L;

			protected Collection<ClassroomSessionDivisionSubjectEvaluationType> classroomSessionDivisionSubjectEvaluationTypeModels;
			protected Boolean useClassroomSessionDivisionSubjectEvaluationTypeModelsOnCreate;
			
			@Override
			public Listener addClassroomSessionDivisionSubjectEvaluationTypeModel(ClassroomSessionDivisionSubjectEvaluationType...models) {
				return null;
			}
			
			@Override
			public Listener setUseClassroomSessionDivisionSubjectEvaluationTypeModelsOnCreate(Boolean value) {
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
