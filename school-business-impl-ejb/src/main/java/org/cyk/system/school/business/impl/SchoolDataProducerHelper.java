package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.inject.Singleton;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.RootDataProducerHelper;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricCollectionIdentifiableGlobalIdentifier;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.mathematics.IntervalDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionDao;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.cdi.BeanAdapter;
import org.joda.time.DateTimeConstants;

import lombok.Getter;
import lombok.Setter;

@Singleton
public class SchoolDataProducerHelper extends AbstractBean implements Serializable {

	private static final long serialVersionUID = -8721724629218389127L;

	private static SchoolDataProducerHelper INSTANCE ;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
	}
	
	public CommonNodeInformations instanciateOneCommonNodeInformations(IntervalCollection intervalCollection,IntervalCollection studentClassroomSessionAveragePromotionScale
			,ReportTemplate reportTemplate,String attendanceTimeDivisionTypeCode,String classroomSessionTimeDivisionTypeCode,String evaluationPassAverage
			,String currentClassroomSessionDivisionIndex){
		CommonNodeInformations commonNodeInformations = new CommonNodeInformations(intervalCollection,studentClassroomSessionAveragePromotionScale,reportTemplate
				,RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class,attendanceTimeDivisionTypeCode),new BigDecimal(evaluationPassAverage));
		commonNodeInformations.setClassroomSessionTimeDivisionType(RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class, classroomSessionTimeDivisionTypeCode));
		commonNodeInformations.setCurrentClassroomSessionDivisionIndex(new Long(currentClassroomSessionDivisionIndex));
		commonNodeInformations.setClassroomSessionDivisionOrderNumberInterval(inject(IntervalDao.class).read(SchoolConstant.Code.Interval.DIVISION_COUNT_BY_CLASSROOM_SESSION));
		return commonNodeInformations;
	}
	
	public void addSubjects(Collection<String> subjectCodes,ArrayList<Subject>[] collections){
		if(collections!=null){
			Collection<Subject> subjects = inject(SubjectDao.class).read(subjectCodes);
			for(Collection<Subject> collection : collections)
				collection.addAll(subjects);
		}
	}
	
	public void instanciateOneClassroomSession(Collection<ClassroomSession> classroomSessions
			,Collection<MetricCollectionIdentifiableGlobalIdentifier> metricCollectionIdentifiableGlobalIdentifiers,AcademicSession academicSession,LevelTimeDivision levelTimeDivision,CommonNodeInformations commonNodeInformations,Object[][] evaluationTypes,Collection<Subject> subjects
			,String[] suffixes,String[] metricCollectionCodes,Boolean studentEvaluationRequired,Boolean studentRankable){
		if(suffixes==null)
			suffixes = new String[]{null};
		for(String suffix : suffixes){
			ClassroomSession classroomSession = new ClassroomSession(academicSession, levelTimeDivision,null,null,commonNodeInformations);
			classroomSession.setSuffix(StringUtils.isBlank(suffix)?null:suffix);
			classroomSession.getGlobalIdentifierCreateIfNull().getExistencePeriod().setFromDate(new Date());
			classroomSession.getExistencePeriod().setToDate(new Date());
			classroomSessions.add(classroomSession);
			classroomSession.getDivisions().setSynchonizationEnabled(Boolean.TRUE);
			
			classroomSession.getDivisions().getCollection().add(createClassroomSessionDivision(metricCollectionIdentifiableGlobalIdentifiers,classroomSession
					,evaluationTypes,subjects,metricCollectionCodes,studentEvaluationRequired,studentRankable,1l));
			classroomSession.getDivisions().getCollection().add(createClassroomSessionDivision(metricCollectionIdentifiableGlobalIdentifiers,classroomSession
					,evaluationTypes,subjects,metricCollectionCodes,studentEvaluationRequired,studentRankable,2l));
			classroomSession.getDivisions().getCollection().add(createClassroomSessionDivision(metricCollectionIdentifiableGlobalIdentifiers,classroomSession
					,evaluationTypes,subjects,metricCollectionCodes,studentEvaluationRequired,studentRankable,3l));
			
		}
		System.out.println(levelTimeDivision.getLevel().getLevelName().getName()+" instanciated");
	}
	/*
	private void grade(Collection<ClassroomSession> classroomSessions,Collection<ClassroomSessionDivision> classroomSessionDivisions
			,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes
			,AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Collection<Subject> subjects){
		grade(classroomSessions, classroomSessionDivisions, classroomSessionDivisionSubjects, subjectEvaluationTypes, academicSession, levelTimeDivision, subjects,new String[]{""});
	}*/
	
	private ClassroomSessionDivision createClassroomSessionDivision(
			Collection<MetricCollectionIdentifiableGlobalIdentifier> metricCollectionIdentifiableGlobalIdentifiers,ClassroomSession classroomSession,Object[][] evaluationTypes,Collection<Subject> subjects
			,String[] metricCollectionCodes,Boolean studentEvaluationRequired,Boolean studentRankable,Long orderNumber){
		ClassroomSessionDivision classroomSessionDivision = new ClassroomSessionDivision(classroomSession,RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class
				,RootConstant.Code.TimeDivisionType.TRIMESTER)
    			,new BigDecimal("1"));
		classroomSessionDivision.setStudentEvaluationRequired(studentEvaluationRequired);
		classroomSessionDivision.setStudentRankable(studentRankable);
		classroomSessionDivision.getExistencePeriod().getNumberOfMillisecond().set(DateTimeConstants.MILLIS_PER_DAY * 45l);
		classroomSessionDivision.getGlobalIdentifierCreateIfNull().getExistencePeriod().setFromDate(new Date());
		classroomSessionDivision.getExistencePeriod().setToDate(new Date());
		classroomSessionDivision.setOrderNumber(orderNumber);
		
		for(MetricCollection metricCollection : inject(MetricCollectionDao.class).read(Arrays.asList(metricCollectionCodes)))
			metricCollectionIdentifiableGlobalIdentifiers.add(new MetricCollectionIdentifiableGlobalIdentifier(metricCollection, classroomSessionDivision,null));
		
		for(Listener listener : Listener.COLLECTION)
			listener.classroomSessionDivisionCreated(classroomSessionDivision);
		
		classroomSessionDivision.getSubjects().setSynchonizationEnabled(Boolean.TRUE);
		if(subjects!=null)
			for(Subject subject : subjects){
				classroomSessionDivision.getSubjects().getCollection().add(
						createClassroomSessionDivisionSubject(classroomSessionDivision,subject,evaluationTypes));
			}
		
		return classroomSessionDivision;
	}
	
	private ClassroomSessionDivisionSubject createClassroomSessionDivisionSubject(ClassroomSessionDivision classroomSessionDivision,Subject subject,Object[][] evaluationTypes){
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(classroomSessionDivision,subject,BigDecimal.ONE,null);
		classroomSessionDivisionSubject.getEvaluationTypes().setSynchonizationEnabled(Boolean.TRUE);
		//classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
		for(Object[] evaluationType : evaluationTypes){
			Object[] infos = evaluationType;
			ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = 
					createSubjectEvaluationType(classroomSessionDivisionSubject, (EvaluationType)infos[0], new BigDecimal((String)infos[1]),new BigDecimal((String)infos[2]));
			classroomSessionDivisionSubject.getEvaluationTypes().getCollection().add(classroomSessionDivisionSubjectEvaluationType);
		}
		return classroomSessionDivisionSubject;
	}
	
	private ClassroomSessionDivisionSubjectEvaluationType createSubjectEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType name,BigDecimal coefficient,BigDecimal maximalValue){
		ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType = new ClassroomSessionDivisionSubjectEvaluationType(subject,name,coefficient,maximalValue);
		for(Listener listener : Listener.COLLECTION)
			listener.classroomSessionDivisionSubjectEvaluationTypeCreated(subjectEvaluationType);
		return subjectEvaluationType;
	}
	
	/**/
	
	public static SchoolDataProducerHelper getInstance() {
		return INSTANCE;
	}
	
	/**/
	
	@Getter @Setter
	public static class ClassroomSessionInfos{
		private ClassroomSession classroomSession;
		private List<ClassroomSessionDivisionInfos> divisions = new ArrayList<>(); 
		
		public ClassroomSessionInfos(ClassroomSession classroomSession) {
			super();
			this.classroomSession = classroomSession;
		}
		
		public ClassroomSessionDivisionInfos division(Integer index){
			return divisions.get(index);
		}
		
		public ClassroomSessionDivisionSubject subject(Integer index,Integer subjectIndex){
			return division(index).getSubjects().get(subjectIndex).getClassroomSessionDivisionSubject();
		}
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionInfos{
		private ClassroomSessionDivision classroomSessionDivision; 
		private List<ClassroomSessionDivisionSubjectInfos> subjects = new ArrayList<>();
		
		public ClassroomSessionDivisionInfos(ClassroomSessionDivision classroomSessionDivision) {
			super();
			this.classroomSessionDivision = classroomSessionDivision;
		}
		
		public ClassroomSessionDivisionSubjectInfos subject(Integer index){
			return subjects.get(index);
		}
		
		public List<ClassroomSessionDivisionSubject> getClassroomSessionDivisionSubjects(){
			List<ClassroomSessionDivisionSubject> list = new ArrayList<>();
			for(ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos : subjects)
				list.add(classroomSessionDivisionSubjectInfos.getClassroomSessionDivisionSubject());
			return list;
		}

		public Collection<ClassroomSessionDivisionSubjectEvaluationType> getEvaluationTypes() {
			Collection<ClassroomSessionDivisionSubjectEvaluationType> evaluationTypes = new ArrayList<>();
			for(ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos : subjects)
				evaluationTypes.addAll(classroomSessionDivisionSubjectInfos.evaluationTypes);
			return evaluationTypes;
		}
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionSubjectInfos{
		private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
		private List<ClassroomSessionDivisionSubjectEvaluationType> evaluationTypes = new ArrayList<>();
		
		public ClassroomSessionDivisionSubjectInfos(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super();
			this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		}
		
		public ClassroomSessionDivisionSubjectEvaluationType evaluationType(Integer index){
			return evaluationTypes.get(index);
		}
		
	}
	
	/**/
	
	public static interface Listener {
		
		Collection<Listener> COLLECTION = new ArrayList<>();
		
		/**/
		
		void classroomSessionDivisionCreated(ClassroomSessionDivision classroomSessionDivision);
		void classroomSessionDivisionSubjectEvaluationTypeCreated(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType);
		
		public static class Adapter extends BeanAdapter implements Listener,Serializable{
			private static final long serialVersionUID = -7938520926769839615L;
			
			/**/
			
			@Override
			public void classroomSessionDivisionCreated(ClassroomSessionDivision classroomSessionDivision) {}
			
			@Override
			public void classroomSessionDivisionSubjectEvaluationTypeCreated(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {}
			
			public static class Default extends Adapter implements Serializable {
				private static final long serialVersionUID = -5680372873034239621L;
				
			}
		}
	}
}
