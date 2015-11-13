package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.impl.AbstractTestHelper;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.joda.time.DateTimeConstants;

@Singleton
public class SchoolBusinessTestHelper extends AbstractTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	private static SchoolBusinessTestHelper INSTANCE;
	
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private SubjectEvaluationBusiness subjectEvaluationBusiness;
	@Inject private SubjectEvaluationTypeBusiness evaluationTypeBusiness;
	
	@Getter @Setter private Boolean coefficientApplied = Boolean.TRUE;
	@Getter @Setter private RankOptions<SortableStudentResults> rankOptions;
	
	@Getter @Setter private List<EvaluationType> evaluationTypes = new ArrayList<>();
	
	/**/
	
	@Override
	protected void initialisation() {
		super.initialisation();
		rankOptions = new RankOptions<>();
        rankOptions.setType(RankType.EXAEQUO); 
        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
	}

	public void createStudentClassroomSessionDivisionSubjects(String[] studentRegistrationCodes,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects){
		for(String studentRegistrationCode : studentRegistrationCodes){
			Student student = studentBusiness.findByRegistrationCode(studentRegistrationCode);
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
				StudentSubject studentSubject = new StudentSubject(student, classroomSessionDivisionSubject);
				studentSubjectBusiness.create(studentSubject);
			}
		}
	}
	
	public void createSubjectEvaluation(SubjectEvaluationType subjectEvaluationType,String[][] details){
		SubjectEvaluation subjectEvaluation = new SubjectEvaluation(subjectEvaluationType,coefficientApplied);
		for(String[] detail : details){
			if(StringUtils.isBlank(detail[1]))
				continue;
			Student student = studentBusiness.findByRegistrationCode(detail[0]);
			StudentSubject studentSubject = studentSubjectBusiness.findByStudentBySubject(student, subjectEvaluation.getType().getSubject());
			subjectEvaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(subjectEvaluation,studentSubject, new BigDecimal(detail[1])));
		}
		subjectEvaluationBusiness.create(subjectEvaluation);
	}

	public void createSubjectEvaluation(ClassroomSessionDivisionSubject subject,EvaluationType evaluationType,String[][] details){
		createSubjectEvaluation(evaluationTypeBusiness.findBySubjectByEvaluationType(subject, evaluationType),details);
	}
	
	public void createStudentClassroomSessionDivisionReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean createFileOnDisk){
		studentClassroomSessionDivisionBusiness.buildReport(classroomSessionDivisions);
		if(Boolean.TRUE.equals(createFileOnDisk)){
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions)
				for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision))
					writeReport(studentClassroomSessionDivisionBusiness.findReport(studentClassroomSessionDivision));
    	}
	}
	
	public void randomValues(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean metric,Boolean attendance){
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions)
			for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
				
				if(Boolean.TRUE.equals(attendance)){
					studentClassroomSessionDivision.getResults().getLectureAttendance().setAttendedDuration(RandomDataProvider.getInstance().randomInt(DateTimeConstants.MILLIS_PER_DAY, DateTimeConstants.MILLIS_PER_DAY*20)*1l);
					studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDuration(RandomDataProvider.getInstance().randomInt(DateTimeConstants.MILLIS_PER_DAY, DateTimeConstants.MILLIS_PER_DAY*20)*1l);
					studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDurationJustified(RandomDataProvider.getInstance().randomInt(DateTimeConstants.MILLIS_PER_DAY, DateTimeConstants.MILLIS_PER_DAY*20)*1l);
					studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision);
				}
				
				if(Boolean.TRUE.equals(metric)){				
					IntervalCollection intervalCollection = studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNodeInformations()
							.getStudentWorkMetricCollection().getValueIntervalCollection();
					RootBusinessLayer.getInstance().getIntervalCollectionBusiness().load(intervalCollection);
					Collection<StudentResultsMetricValue> studentResultsMetricValues = SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness()
							.findByStudentResults(studentClassroomSessionDivision.getResults());
					for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues){
						studentResultsMetricValue.getMetricValue().setValue(new BigDecimal(RandomDataProvider.getInstance().randomInt(intervalCollection.getLowestValue().intValue(), intervalCollection.getHighestValue().intValue())));
					}
					studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision,studentResultsMetricValues);
				}
				
			}
	}
	
	/**/
	
	public void assertClassroomSessionDivisionSubjectAverage(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details){
		Collection<StudentSubject> studentSubjects = studentSubjectBusiness.average(Arrays.asList(classroomSessionDivisionSubject), Boolean.TRUE);
		for(StudentSubject studentSubject : studentSubjects){
			for(String[] detail : details)
				if(detail[0].equals(studentSubject.getStudent().getRegistration().getCode())){
					assertBigDecimalEquals("Average of "+studentSubject.getStudent(), detail[1], studentSubject.getResults().getEvaluationSort().getAverage().getValue());
				}
		}
	}
	
	public void assertClassroomSessionDivisionSubjectRank(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details,RankOptions<SortableStudentResults> rankOptions){
		Collection<StudentSubject> studentSubjects = studentSubjectBusiness.average(Arrays.asList(classroomSessionDivisionSubject), Boolean.TRUE);
		studentSubjectBusiness.rank(studentSubjects,rankOptions);
		for(StudentSubject studentSubject : studentSubjects){
			for(String[] detail : details)
				if(detail[0].equals(studentSubject.getStudent().getRegistration().getCode())){
					assertEquals("Rank Value of "+studentSubject.getStudent(), detail[1], studentSubject.getResults().getEvaluationSort().getRank().getValue().toString());
					assertEquals("Rank Exaequo of "+studentSubject.getStudent(), detail.length>2?detail[2]:"false", 
							studentSubject.getResults().getEvaluationSort().getRank().getExaequo()==null?"false":studentSubject.getResults().getEvaluationSort().getRank().getExaequo());
				}
		}
	}
	
	/*
	public void assertClassroomSessionDivisionSubjectAfterEvaluation(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType, String[][] details,RankOptions<SortableStudentResults> rankOptions){
		createSubjectEvaluations(classroomSessionDivisionSubject, evaluationType,extract(details, 1));
    	assertClassroomSessionDivisionSubjectAverage(classroomSessionDivisionSubject, extract(details, 2));    	
    	assertClassroomSessionDivisionSubjectRank(classroomSessionDivisionSubject,extract(details, 3),rankOptions);
	}
	
	public void assertClassroomSessionDivisionSubjectAfterEvaluation(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,EvaluationType evaluationType, String[][] details){
		assertClassroomSessionDivisionSubjectAfterEvaluation(classroomSessionDivisionSubject, evaluationType, details, rankOptions);
	}*/
	
	public void createSubjectEvaluations(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details,Boolean assertAverage,Boolean assertRank){
		int i = 1;
		for(EvaluationType evaluationType : evaluationTypes){
			createSubjectEvaluation(classroomSessionDivisionSubject, evaluationType, extract(details, i++));
		}
		
		asserts(classroomSessionDivisionSubject, details, evaluationTypes.size()+1, assertAverage, assertRank);
	}
	
	public void createSubjectEvaluations(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details){
		createSubjectEvaluations(classroomSessionDivisionSubject,details, Boolean.FALSE, Boolean.FALSE);
	}
	
	public void asserts(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details,Integer index,Boolean assertAverage,Boolean assertRank){
		if(Boolean.TRUE.equals(assertAverage) || Boolean.TRUE.equals(assertRank)){
			if(Boolean.TRUE.equals(assertAverage))
				assertClassroomSessionDivisionSubjectAverage(classroomSessionDivisionSubject, extract(details, index++));    	
			if(Boolean.TRUE.equals(assertRank))
				assertClassroomSessionDivisionSubjectRank(classroomSessionDivisionSubject,extract(details, index),rankOptions);
		}
	}
	
	public void asserts(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details,Boolean assertAverage,Boolean assertRank){
		asserts(classroomSessionDivisionSubject, details, 1, assertAverage, assertRank);
	}
	
	public void asserts(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details){
		asserts(classroomSessionDivisionSubject, details, Boolean.TRUE, Boolean.TRUE);
	}
	
	private String[][] extract(String[][] details,Integer columnIndex){
		String[][] data = new String[details.length][2];
		for(int rowIndex = 0;rowIndex<details.length;rowIndex++){
			data[rowIndex][0] = details[rowIndex][0];
			data[rowIndex][1] = details[rowIndex][columnIndex];
		}
		return data;
	}
	
	/**/
	public static SchoolBusinessTestHelper getInstance() {
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

		public Collection<SubjectEvaluationType> getEvaluationTypes() {
			Collection<SubjectEvaluationType> evaluationTypes = new ArrayList<>();
			for(ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos : subjects)
				evaluationTypes.addAll(classroomSessionDivisionSubjectInfos.evaluationTypes);
			return evaluationTypes;
		}
		
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionSubjectInfos{
		private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
		private List<SubjectEvaluationType> evaluationTypes = new ArrayList<>();
		
		public ClassroomSessionDivisionSubjectInfos(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super();
			this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		}
		
		public SubjectEvaluationType evaluationType(Integer index){
			return evaluationTypes.get(index);
		}
		
	}
}
