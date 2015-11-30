package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.impl.AbstractTestHelper;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.RootRandomDataProvider;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationTypeBusiness;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.utility.common.generator.RandomDataProvider;

@Singleton
public class SchoolBusinessTestHelper extends AbstractTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	private static SchoolBusinessTestHelper INSTANCE;
	
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionBusiness studentClassroomSessionBusiness;
	@Inject private SubjectEvaluationBusiness subjectEvaluationBusiness;
	@Inject private SubjectEvaluationTypeBusiness evaluationTypeBusiness;
	
	@Getter @Setter private Boolean coefficientApplied = Boolean.TRUE;
	@Getter @Setter private RankOptions<SortableStudentResults> rankOptions;
	
	@Getter @Setter private List<EvaluationType> evaluationTypes = new ArrayList<>();
	
	/**/
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
		rankOptions = new RankOptions<>();
        rankOptions.setType(RankType.EXAEQUO); 
        rankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
	}
	
	public void randomSetActor(Boolean classCoordinator,Boolean teacher){
		if(Boolean.TRUE.equals(classCoordinator)){
			Collection<ClassroomSession> classroomSessions = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll();
			for(ClassroomSession classroomSession : classroomSessions)
				classroomSession.setCoordinator(RootRandomDataProvider.getInstance().oneFromDatabase(Teacher.class));
			SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().update(classroomSessions);
		}
		if(Boolean.TRUE.equals(teacher)){
			Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findAll();
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects)
				classroomSessionDivisionSubject.setTeacher(RootRandomDataProvider.getInstance().oneFromDatabase(Teacher.class));
			SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().update(classroomSessionDivisionSubjects);
		}
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Integer generateStudentInClassroomSessionCount,Integer studentByClassroomSessionCount){
		Collection<ClassroomSession> classroomSessions = generateStudentInClassroomSessionCount==null?
				SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll():
					SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findManyRandomly(generateStudentInClassroomSessionCount);
		return createStudentClassroomSessions(SchoolBusinessLayer.getInstance().getStudentBusiness().findManyRandomly(studentByClassroomSessionCount),classroomSessions);
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Integer studentByClassroomSessionCount,Collection<ClassroomSession> classroomSessions){
		return createStudentClassroomSessions(SchoolBusinessLayer.getInstance().getStudentBusiness().findManyRandomly(studentByClassroomSessionCount),classroomSessions);
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Collection<Student> students,Collection<ClassroomSession> classroomSessions){
		Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		
		for(ClassroomSession classroomSession : classroomSessions)
			for(Student student : students)
				studentClassroomSessions.add(new StudentClassroomSession(student, classroomSession));
				
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().create(studentClassroomSessions);
		return classroomSessions;
	}
	
	public void createSubjectEvaluations(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Boolean coefficientApplied){
		Collection<SubjectEvaluation> subjectEvaluations = new ArrayList<>();
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			Collection<SubjectEvaluationType> subjectEvaluationTypes = SchoolBusinessLayer.getInstance().getSubjectEvaluationTypeBusiness().findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
			Collection<StudentSubject> studentSubjects = SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().findBySubject(classroomSessionDivisionSubject);
			if(studentSubjects.isEmpty())
				continue;
			for(SubjectEvaluationType subjectEvaluationType : subjectEvaluationTypes){
				SubjectEvaluation subjectEvaluation = new SubjectEvaluation(subjectEvaluationType, coefficientApplied);
				subjectEvaluations.add(subjectEvaluation);
				for(StudentSubject studentSubject : studentSubjects ){
					subjectEvaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(subjectEvaluation, studentSubject
							, new BigDecimal(RandomDataProvider.getInstance().randomInt(0, subjectEvaluationType.getMaximumValue().intValue()))));
				}
			}
		}
		SchoolBusinessLayer.getInstance().getSubjectEvaluationBusiness().create(subjectEvaluations);
	}
	public void createSubjectEvaluations(Boolean coefficientApplied){
		createSubjectEvaluations(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().findAll(), coefficientApplied);
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
	
	public void createStudentClassroomSessionDivisionReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Set<Integer> classroomSessionDivisionIndexes,Boolean createFileOnDisk){
		studentClassroomSessionDivisionBusiness.buildReport(classroomSessionDivisions);
		if(Boolean.TRUE.equals(createFileOnDisk)){
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions)
				if(classroomSessionDivisionIndexes==null || classroomSessionDivisionIndexes.isEmpty()
					|| (classroomSessionDivisionIndexes.contains(classroomSessionDivision.getIndex().intValue())) )
					for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
						studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.find(studentClassroomSessionDivision.getIdentifier());
						assertThat("Report built", studentClassroomSessionDivision.getResults().getReport()!=null);
						writeReport(studentClassroomSessionDivisionBusiness.findReport(studentClassroomSessionDivision));
					}
    	}
	}
	public void createStudentClassroomSessionDivisionReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean createFileOnDisk){
		createStudentClassroomSessionDivisionReport(classroomSessionDivisions, null, createFileOnDisk);
	}
	public void createStudentClassroomSessionDivisionReport(Boolean createFileOnDisk){
		createStudentClassroomSessionDivisionReport(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findAll(),createFileOnDisk);
	}
	public void createStudentClassroomSessionDivisionReport(ClassroomSessionDivision classroomSessionDivision,Boolean createFileOnDisk){
		createStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivision), createFileOnDisk);
	}
	
	public void randomValues(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean metric,Boolean attendance,Boolean appreciation){
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			Long t = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findCommonNodeInformations(classroomSessionDivision.getClassroomSession()).getAttendanceTimeDivisionType().getDuration();
			for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
				
				if(Boolean.TRUE.equals(attendance)){
					studentClassroomSessionDivision.getResults().getLectureAttendance().setAttendedDuration(randomDataProvider.randomInt(0, 1)*t);
					studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDuration(randomDataProvider.randomInt(0, 1)*t);
					studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDurationJustified(randomDataProvider.randomInt(0, 1)*t);
					studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision);
				}
				
				if(Boolean.TRUE.equals(metric)){				
					IntervalCollection intervalCollection = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findCommonNodeInformations(classroomSessionDivision.getClassroomSession())
							.getStudentWorkMetricCollection().getValueIntervalCollection();
					RootBusinessLayer.getInstance().getIntervalCollectionBusiness().load(intervalCollection);
					Collection<StudentResultsMetricValue> studentResultsMetricValues = SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness()
							.findByStudentResults(studentClassroomSessionDivision.getResults());
					for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues){
						studentResultsMetricValue.getMetricValue().setValue(new BigDecimal(RandomDataProvider.getInstance().randomInt(intervalCollection.getLowestValue().intValue(), intervalCollection.getHighestValue().intValue())));
					}
					studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision,studentResultsMetricValues);
				}
				
				if(Boolean.TRUE.equals(appreciation)){
					studentClassroomSessionDivision.getResults().setAppreciation(RandomStringUtils.randomAlphabetic(50));
					studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision);
				}
			}
		}
	}
	public void randomValues(Boolean metric,Boolean attendance,Boolean appreciation){
		randomValues(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findAll(), metric, attendance, appreciation);
	}
	
	public StudentClassroomSession createStudentClassroomSession(String registrationCode,ClassroomSession classroomSession,Object[][] expected){
		StudentClassroomSession studentClassroomSession = new StudentClassroomSession(studentBusiness.findByRegistrationCode(registrationCode), classroomSession);
		studentClassroomSession = studentClassroomSessionBusiness.create(studentClassroomSession);
		assertStudentClassroomSession(studentClassroomSession, expected);
		return studentClassroomSession;
	}
	
	public void createStudentClassroomSessions(String[] registrationCodes,ClassroomSession classroomSession,Object[][] expected){
		for(String code : registrationCodes)
			createStudentClassroomSession(code, classroomSession, expected);
	}
	
	public StudentSubject createStudentSubject(String registrationCode,ClassroomSessionDivisionSubject classroomSessionDivisionSubject,Object[][] expected){
		StudentSubject studentSubject = new StudentSubject(studentBusiness.findByRegistrationCode(registrationCode), classroomSessionDivisionSubject);
		studentSubject = studentSubjectBusiness.create(studentSubject);
		assertStudentClassroomSession(studentClassroomSessionBusiness.findByStudentByClassroomSession(studentSubject.getStudent()
				, studentSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()), expected);
		return studentSubject;
	}
	
	public void assertStudentClassroomSession(StudentClassroomSession studentClassroomSession,Object[][] expected){
		assertEquals("Student classroom session division count", expected.length, 
				SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
		
		int i=0;
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()))
			assertStudentClassroomSessionDivision(studentClassroomSessionDivision, expected[i++]);
		
	}
	
	public void assertStudentClassroomSessionDivision(StudentClassroomSessionDivision studentClassroomSessionDivision,Object[] expected){
		assertEquals("Student classroom session division subject count", (Integer)expected[0], 
				SchoolBusinessLayer.getInstance().getStudentSubjectBusiness()
				.findByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent(), studentClassroomSessionDivision.getClassroomSessionDivision()).size());
	}
	
	public void updateStudentClassroomSession(StudentClassroomSession studentClassroomSession,ClassroomSession classroomSession){
		Integer oldStudentClassroomSessionDivisionCount = SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size();
		
		studentClassroomSession.setClassroomSession(classroomSession);
		
		studentClassroomSessionBusiness.update(studentClassroomSession);
		
		Integer newStudentClassroomSessionDivisionCount = SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size();
		
		assertEquals("Student classroom session division count", oldStudentClassroomSessionDivisionCount, newStudentClassroomSessionDivisionCount);
	}
	
	public void deleteStudentClassroomSession(StudentClassroomSession studentClassroomSession,Object[][] expected){
		studentClassroomSessionBusiness.delete(studentClassroomSession);
		if(expected==null){
			assertEquals("Student classroom session division count", 0, SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
					.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
			
			assertEquals("Student classroom session division subject count", 0, SchoolBusinessLayer.getInstance().getStudentSubjectBusiness()
					.findByStudentByClassroomSessionDivision(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
		}else{
			assertStudentClassroomSession(studentClassroomSession, expected);
		}
		
	}
	
	public void deleteStudentClassroomSession(StudentClassroomSession studentClassroomSession){
		deleteStudentClassroomSession(studentClassroomSession, null);
	}
	
	public void updateStudentClassroomSessionDivision(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues,String[] values) {
		int i = 0;
		for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues)
			studentResultsMetricValue.getMetricValue().setValue(new BigDecimal(values[i++]));
		SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().update(studentClassroomSessionDivision, new ArrayList<>(studentResultsMetricValues));
		Collection<StudentResultsMetricValue> updateStudentResultsMetricValues = SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResults(studentClassroomSessionDivision.getResults());
		assertEquals("Student classroom session division metrics count", studentResultsMetricValues.size(), updateStudentResultsMetricValues.size());
		
		for(StudentResultsMetricValue u : studentResultsMetricValues)
			for(StudentResultsMetricValue s : updateStudentResultsMetricValues)
				if(u.getIdentifier().equals(s.getIdentifier())){
					assertEquals("Student results identifier", u.getIdentifier(), s.getIdentifier());
					assertEquals("Student results metric value", u.getMetricValue().getValue(), s.getMetricValue().getValue());
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
	
	public void simulate(SchoolBusinessSimulationParameters parameters){
		System.out.println("School business simulation started");
		
    	System.out.println("Creating teachers");
		RootRandomDataProvider.getInstance().createActor(Teacher.class, parameters.getTeacherCount());
		System.out.println("Creating students");
		RootRandomDataProvider.getInstance().createActor(Student.class, parameters.getStudentCount());
    	
		System.out.println("Setting class coordinators , subject teachers");
    	randomSetActor(Boolean.TRUE, Boolean.TRUE);
    	
    	System.out.println("Creating student classroom session");
    	Collection<ClassroomSession> classroomSessions;
    	if(parameters.getGeneratedClassroomSessionCountByLevel()==null)
    		classroomSessions = createStudentClassroomSessions(parameters.getGeneratedStudentInClassroomSessionCount(),
    			parameters.getStudentByClassroomSessionCount());
    	else{
    		classroomSessions = new ArrayList<>();
    		Set<String> levelNames = new HashSet<>();
    		for(ClassroomSession classroomSession : SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll()){
    			if(levelNames.add(classroomSession.getLevelTimeDivision().getLevel().getName().getCode())){
    				classroomSessions.add(classroomSession);
    			}
    		}
    		createStudentClassroomSessions(parameters.getStudentByClassroomSessionCount(),classroomSessions);
    	}
    	System.out.println("Creating subject evaluations");
    	createSubjectEvaluations(coefficientApplied);
    	
    	System.out.println("Setting student metric , attendance , appreciation");
    	randomValues(Boolean.TRUE,Boolean.TRUE,Boolean.TRUE);
    	
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>();
    	for(ClassroomSession classroomSession : classroomSessions)
    		classroomSessionDivisions.addAll(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSession(classroomSession));
    	System.out.println("Creating student classroom session reports");
		createStudentClassroomSessionDivisionReport(classroomSessionDivisions,parameters.getClassroomSessionDivisionIndexes(),Boolean.TRUE);
		
		System.out.println("School business simulation ended");
	}
	
	/**/
	public static SchoolBusinessTestHelper getInstance() {
		return INSTANCE;
	}

	/**/
	
	@Getter @Setter
	public static class SchoolBusinessSimulationParameters{
		private Integer teacherCount=1,studentCount=1,generatedStudentInClassroomSessionCount=1,studentByClassroomSessionCount=1,
			generatedClassroomSessionCountByLevel=1;
		private Set<Integer> classroomSessionDivisionIndexes = new HashSet<>();
		
		private Boolean createStudentClassroomSessionForAllLevel;
		private Boolean createFileOnDiskOfOneStudentClassroomSessionDivisionReportForAllLevel;
		
	}
	
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
