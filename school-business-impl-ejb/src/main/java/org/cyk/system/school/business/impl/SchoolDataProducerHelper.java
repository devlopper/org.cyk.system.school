package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.RootDataProducerHelper;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.utility.common.cdi.AbstractBean;
import org.joda.time.DateTimeConstants;

@Singleton
public class SchoolDataProducerHelper extends AbstractBean implements Serializable {

	private static final long serialVersionUID = -8721724629218389127L;

	private static SchoolDataProducerHelper INSTANCE ;
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
	}
	
	public CommonNodeInformations instanciateOneCommonNodeInformations(IntervalCollection intervalCollection,ReportTemplate reportTemplate
			,String attendanceTimeDivisionTypeCode,String classroomSessionTimeDivisionTypeCode,String currentClassroomSessionDivisionIndex){
		CommonNodeInformations commonNodeInformations = new CommonNodeInformations(intervalCollection,reportTemplate,RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class,attendanceTimeDivisionTypeCode));
		commonNodeInformations.setClassroomSessionTimeDivisionType(RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class, classroomSessionTimeDivisionTypeCode));
		commonNodeInformations.setCurrentClassroomSessionDivisionIndex(new Byte(currentClassroomSessionDivisionIndex));
		return commonNodeInformations;
	}
	
	public Subject instanciateOneSubject(String name,ArrayList<Subject>[] collections){
		Subject subject = SchoolBusinessLayer.getInstance().getSubjectBusiness().instanciateOne(name);
		if(collections!=null)
			for(Collection<Subject> collection : collections)
				collection.add(subject);
		return subject;
	}
	
	public Subject createOneSubject(String name,ArrayList<Subject>[] collections){
		return (Subject) RootBusinessLayer.getInstance().getGenericBusiness().create(instanciateOneSubject(name, collections));
	}
	
	public Collection<ClassroomSessionInfos> instanciateOneClassroomSession(Collection<ClassroomSession> classroomSessions,Collection<ClassroomSessionDivision> classroomSessionDivisions
			,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes
			,AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Object[][] evaluationTypes,Collection<Subject> subjects,Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections
			,MetricCollection[] studentMetricCollections,String[] suffixes,Boolean studentEvaluationRequired,Boolean studentRankable){
		if(suffixes==null)
			suffixes = new String[]{null};
		Collection<ClassroomSessionInfos> classroomSessionInfosCollection = new ArrayList<>();
		for(String suffix : suffixes){
			ClassroomSession classroomSession = new ClassroomSession(academicSession, levelTimeDivision,null);
			classroomSession.setSuffix(StringUtils.isBlank(suffix)?null:suffix);
			classroomSession.getPeriod().setFromDate(new Date());
			classroomSession.getPeriod().setToDate(new Date());
			classroomSessions.add(classroomSession);
			ClassroomSessionInfos classroomSessionInfos = new ClassroomSessionInfos(classroomSession);
			classroomSessionInfosCollection.add(classroomSessionInfos);
			classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionInfos.getClassroomSession()
					,evaluationTypes,subjects,classroomSessionDivisionStudentsMetricCollections,studentMetricCollections,studentEvaluationRequired,studentRankable));
			classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionInfos.getClassroomSession()
					,evaluationTypes,subjects,classroomSessionDivisionStudentsMetricCollections,studentMetricCollections,studentEvaluationRequired,studentRankable));
			classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionInfos.getClassroomSession()
					,evaluationTypes,subjects,classroomSessionDivisionStudentsMetricCollections,studentMetricCollections,studentEvaluationRequired,studentRankable));
		}
		return classroomSessionInfosCollection;
	}
	/*
	private void grade(Collection<ClassroomSession> classroomSessions,Collection<ClassroomSessionDivision> classroomSessionDivisions
			,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes
			,AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Collection<Subject> subjects){
		grade(classroomSessions, classroomSessionDivisions, classroomSessionDivisionSubjects, subjectEvaluationTypes, academicSession, levelTimeDivision, subjects,new String[]{""});
	}*/
	
	private ClassroomSessionDivisionInfos createClassroomSessionDivision(Collection<ClassroomSessionDivision> classroomSessionDivisions
			,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes
			,ClassroomSession classroomSession,Object[][] evaluationTypes,Collection<Subject> subjects,Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections
			,MetricCollection[] studentMetricCollections,Boolean studentEvaluationRequired,Boolean studentRankable){
		ClassroomSessionDivision classroomSessionDivision = new ClassroomSessionDivision(classroomSession,RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER)
    			,new BigDecimal("1"));
		classroomSessionDivision.setStudentEvaluationRequired(studentEvaluationRequired);
		classroomSessionDivision.setStudentRankable(studentRankable);
		classroomSessionDivision.setDuration(DateTimeConstants.MILLIS_PER_DAY * 45l);
		classroomSessionDivisions.add(classroomSessionDivision);
		classroomSessionDivision.getPeriod().setFromDate(new Date());
		classroomSessionDivision.getPeriod().setToDate(new Date());
		ClassroomSessionDivisionInfos classroomSessionDivisionInfos = new ClassroomSessionDivisionInfos(classroomSessionDivision);
		
		if(subjects!=null)
			for(Subject subject : subjects){
				classroomSessionDivisionInfos.getSubjects().add(createClassroomSessionDivisionSubject(classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionDivision,subject,evaluationTypes));
			}
		
		if(studentMetricCollections!=null)
			for(MetricCollection metricCollection : studentMetricCollections)
				classroomSessionDivisionStudentsMetricCollections.add(new ClassroomSessionDivisionStudentsMetricCollection(classroomSessionDivision, metricCollection));
    	
		return classroomSessionDivisionInfos;
	}
	
	private ClassroomSessionDivisionSubjectInfos createClassroomSessionDivisionSubject(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes,ClassroomSessionDivision classroomSessionDivision,Subject subject,Object[][] evaluationTypes){
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(classroomSessionDivision,subject,BigDecimal.ONE,null);
		classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
		ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos = new ClassroomSessionDivisionSubjectInfos(classroomSessionDivisionSubject);
		for(Object[] evaluationType : evaluationTypes){
			Object[] infos = evaluationType;
			classroomSessionDivisionSubjectInfos.getEvaluationTypes().add(createSubjectEvaluationType(subjectEvaluationTypes,classroomSessionDivisionSubject, (EvaluationType)infos[0], new BigDecimal((String)infos[1]),new BigDecimal((String)infos[2])));
		}
		return classroomSessionDivisionSubjectInfos;
	}
	
	private ClassroomSessionDivisionSubjectEvaluationType createSubjectEvaluationType(Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes,ClassroomSessionDivisionSubject subject,EvaluationType name,BigDecimal coefficient,BigDecimal maximalValue){
		ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType = new ClassroomSessionDivisionSubjectEvaluationType(subject,name,coefficient,maximalValue);
		subjectEvaluationTypes.add(subjectEvaluationType);
		return subjectEvaluationType;
	}
	
	public LevelTimeDivision createLevelTimeDivision(String levelCode,String levelName,LevelGroup levelGroup,CommonNodeInformations commonNodeInformations,Integer index){
		commonNodeInformations.setAggregateAttendance(Boolean.FALSE);
		LevelName _levelName = RootDataProducerHelper.getInstance().createEnumeration(LevelName.class,levelCode,levelName);
		_levelName.setNodeInformations(commonNodeInformations);
		return RootDataProducerHelper.getInstance().create(new LevelTimeDivision(RootDataProducerHelper.getInstance().create(new Level(levelGroup,_levelName))
				, RootDataProducerHelper.getInstance().getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR),index));
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
	
}
