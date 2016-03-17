package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
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
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.RootRandomDataProvider;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionStudentsMetricCollectionDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.cyk.utility.common.FileExtension;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.cyk.utility.common.test.TestEnvironmentListener.Try;

@Singleton
public class SchoolBusinessTestHelper extends AbstractBusinessTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	private static SchoolBusinessTestHelper INSTANCE;
	
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionBusiness studentClassroomSessionBusiness;
	@Inject private EvaluationBusiness subjectEvaluationBusiness;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeBusiness evaluationTypeBusiness;
	
	@Inject private LevelTimeDivisionDao levelTimeDivisionDao;
	@Inject private TimeDivisionTypeDao timeDivisionTypeDao;
	@Inject private SubjectDao subjectDao;
	@Inject private TeacherDao teacherDao;
	@Inject private ClassroomSessionDivisionStudentsMetricCollectionDao classroomSessionDivisionStudentsMetricCollectionDao;
	
	@Inject private SchoolBusinessLayer schoolBusinessLayer;
	
	@Getter @Setter private Boolean studentSubjectCascadeBottomUpOnCreate,studentSubjectCascadeTopDownOnCreate;
	@Getter @Setter private RankOptions<SortableStudentResults> rankOptions;
	
	@Getter @Setter private List<EvaluationType> evaluationTypes = new ArrayList<>();
	@Getter @Setter private Object[][] customClassroomSessionDivisionSubjectEvaluationTypeInfos;
	
	@Getter @Setter private Integer appreciationLenght = 300;
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
			Collection<ClassroomSession> classroomSessions = schoolBusinessLayer.getClassroomSessionBusiness().findAll();
			for(ClassroomSession classroomSession : classroomSessions)
				classroomSession.setCoordinator(RootRandomDataProvider.getInstance().oneFromDatabase(Teacher.class));
			schoolBusinessLayer.getClassroomSessionBusiness().update(classroomSessions);
		}
		if(Boolean.TRUE.equals(teacher)){
			Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = schoolBusinessLayer.getClassroomSessionDivisionSubjectBusiness().findAll();
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects)
				classroomSessionDivisionSubject.setTeacher(RootRandomDataProvider.getInstance().oneFromDatabase(Teacher.class));
			schoolBusinessLayer.getClassroomSessionDivisionSubjectBusiness().update(classroomSessionDivisionSubjects);
		}
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Integer generateStudentInClassroomSessionCount,Integer studentByClassroomSessionCount){
		Collection<ClassroomSession> classroomSessions = generateStudentInClassroomSessionCount==null?
				schoolBusinessLayer.getClassroomSessionBusiness().findAll():
					schoolBusinessLayer.getClassroomSessionBusiness().findManyRandomly(generateStudentInClassroomSessionCount);
		return createStudentClassroomSessions(schoolBusinessLayer.getStudentBusiness().findManyRandomly(studentByClassroomSessionCount),classroomSessions);
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Integer studentByClassroomSessionCount,Collection<ClassroomSession> classroomSessions){
		return createStudentClassroomSessions(schoolBusinessLayer.getStudentBusiness().findManyRandomly(studentByClassroomSessionCount),classroomSessions);
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Collection<Student> students,Collection<ClassroomSession> classroomSessions){
		Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		
		for(ClassroomSession classroomSession : classroomSessions)
			for(Student student : students)
				studentClassroomSessions.add(new StudentClassroomSession(student, classroomSession));
				
		schoolBusinessLayer.getStudentClassroomSessionBusiness().create(studentClassroomSessions);
		return classroomSessions;
	}
	
	public void deleteStudentClassroomSessions(String[] studentRegistrationCode,ClassroomSessionDivision classroomSessionDivision){
		//schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().del
	}
	
	public void createSubjectEvaluations(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Boolean coefficientApplied){
		Collection<Evaluation> subjectEvaluations = new ArrayList<>();
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = schoolBusinessLayer.getSubjectEvaluationTypeBusiness().findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
			Collection<StudentSubject> studentSubjects = schoolBusinessLayer.getStudentSubjectBusiness().findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
			if(studentSubjects.isEmpty())
				continue;
			for(ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType : subjectEvaluationTypes){
				Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType);
				subjectEvaluation.setCoefficientApplied(coefficientApplied);
				subjectEvaluations.add(subjectEvaluation);
				for(StudentSubject studentSubject : studentSubjects ){
					subjectEvaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(subjectEvaluation, studentSubject
							, new BigDecimal(RandomDataProvider.getInstance().randomInt(0, subjectEvaluationType.getMaximumValue().intValue()))));
				}
			}
		}
		System.out.println("Creating "+subjectEvaluations.size()+" evaluation(s)");
		schoolBusinessLayer.getEvaluationBusiness().create(subjectEvaluations);
	}
	public void createSubjectEvaluations(Boolean coefficientApplied){
		createSubjectEvaluations(schoolBusinessLayer.getClassroomSessionDivisionSubjectBusiness().findAll(), coefficientApplied);
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
	
	public void createSubjectEvaluation(ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType,String[][] details){
		Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType); 
		for(String[] detail : details){
			if(StringUtils.isBlank(detail[1]))
				continue;
			Student student = studentBusiness.findByRegistrationCode(detail[0]);
			StudentSubject studentSubject = studentSubjectBusiness.findByStudentByClassroomSessionDivisionSubject(student, subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject());
			subjectEvaluation.getStudentSubjectEvaluations().add(new StudentSubjectEvaluation(subjectEvaluation,studentSubject, new BigDecimal(detail[1])));
		}
		//debug(subjectEvaluation);
		//debug(subjectEvaluation.getStudentSubjectEvaluations().iterator().next());
		subjectEvaluationBusiness.create(subjectEvaluation);
	}

	public void createSubjectEvaluation(ClassroomSessionDivisionSubject subject,EvaluationType evaluationType,String[][] details){
		createSubjectEvaluation(evaluationTypeBusiness.findBySubjectByEvaluationType(subject, evaluationType),details);
	}
	
	public void createStudentClassroomSessionDivisionReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Set<Integer> classroomSessionDivisionIndexes,Boolean createFileOnDisk){
		System.out.println("Building report of "+classroomSessionDivisions.size()+" classroom session divisions");
		studentClassroomSessionDivisionBusiness.buildReport(classroomSessionDivisions);
		if(Boolean.TRUE.equals(createFileOnDisk)){
			Collection<File> files = new ArrayList<>();
			for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
				classroomSessionDivision = schoolBusinessLayer.getClassroomSessionDivisionBusiness().find(classroomSessionDivision.getIdentifier());
				if(classroomSessionDivisionIndexes==null || classroomSessionDivisionIndexes.isEmpty()
					|| (classroomSessionDivisionIndexes.contains(classroomSessionDivision.getIndex().intValue())) ){
					for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
						studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.find(studentClassroomSessionDivision.getIdentifier());
						if( (studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired() && !SchoolBusinessLayer.getInstance().getStudentSubjectEvaluationBusiness()
								.findByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent()
										, studentClassroomSessionDivision.getClassroomSessionDivision()).isEmpty()) || !studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired()){
							assertThat("Report of "+studentClassroomSessionDivision.getStudent()+" built", studentClassroomSessionDivision.getResults().getReport()!=null);
							System.out.println("Writing report of : "+studentClassroomSessionDivision.getStudent()+" , "+studentClassroomSessionDivision.getClassroomSessionDivision()+" , "+studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession());
							writeReport(studentClassroomSessionDivisionBusiness.findReport(studentClassroomSessionDivision));
							files.add(studentClassroomSessionDivision.getResults().getReport());	
						}
					}
				}
			}
			writeStream(RootBusinessLayer.getInstance().getFileBusiness().merge(files, FileExtension.PDF), "allreports_"+System.currentTimeMillis(), "pdf");
    	}
	}
	public void createStudentClassroomSessionDivisionReport(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean createFileOnDisk){
		createStudentClassroomSessionDivisionReport(classroomSessionDivisions, null, createFileOnDisk);
	}
	public void createStudentClassroomSessionDivisionReport(Boolean createFileOnDisk){
		createStudentClassroomSessionDivisionReport(schoolBusinessLayer.getClassroomSessionDivisionBusiness().findAll(),createFileOnDisk);
	}
	public void createStudentClassroomSessionDivisionReport(ClassroomSessionDivision classroomSessionDivision,Boolean createFileOnDisk){
		createStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivision), createFileOnDisk);
	}
	
	public void randomValues(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean metric,Boolean attendance,Boolean appreciation){
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			Long t = schoolBusinessLayer.getClassroomSessionBusiness().findCommonNodeInformations(classroomSessionDivision.getClassroomSession()).getAttendanceTimeDivisionType().getDuration();
			for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
				studentClassroomSessionDivisions.add(studentClassroomSessionDivision);
				
				if(Boolean.TRUE.equals(attendance)){
					studentClassroomSessionDivision.getResults().getLectureAttendance().setAttendedDuration(randomDataProvider.randomInt(0, 1)*t);
					studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDuration(randomDataProvider.randomInt(0, 1)*t);
					studentClassroomSessionDivision.getResults().getLectureAttendance().setMissedDurationJustified(randomDataProvider.randomInt(0, 1)*t);
					//studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision);
				}
				/*
				if(Boolean.TRUE.equals(metric)){
					Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = 
							classroomSessionDivisionStudentsMetricCollectionDao.readByClassroomSessionDivision(classroomSessionDivision);
					for(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection : classroomSessionDivisionStudentsMetricCollections){
						IntervalCollection intervalCollection = classroomSessionDivisionStudentsMetricCollection.getMetricCollection().getValueIntervalCollection();
						RootBusinessLayer.getInstance().getIntervalCollectionBusiness().load(intervalCollection);
						Collection<StudentResultsMetricValue> studentResultsMetricValues = schoolBusinessLayer.getStudentResultsMetricValueBusiness()
								.findByStudentResults(studentClassroomSessionDivision.getResults());
						for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues){
							studentResultsMetricValue.getMetricValue().setNumberValue(new BigDecimal(RandomDataProvider.getInstance().randomInt(intervalCollection.getLowestValue().intValue(), intervalCollection.getHighestValue().intValue())));
							studentResultsMetricValue.getMetricValue().setStringValue(RandomStringUtils.randomAlphabetic(1));
						}
						studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision,studentResultsMetricValues);	
					}
				}
				*/
				if(Boolean.TRUE.equals(appreciation)){
					studentClassroomSessionDivision.getResults().setAppreciation(RandomStringUtils.randomAlphabetic(appreciationLenght));
					//studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision);
				}
			}			
		}
		
		System.out.println("Updating "+studentClassroomSessionDivisions.size()+" student classroom session division(s)");
		studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivisions);
		
		if(Boolean.TRUE.equals(metric)){
			for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
				Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = 
						classroomSessionDivisionStudentsMetricCollectionDao.readByClassroomSessionDivision(studentClassroomSessionDivision.getClassroomSessionDivision());
				for(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection : classroomSessionDivisionStudentsMetricCollections){
					IntervalCollection intervalCollection = classroomSessionDivisionStudentsMetricCollection.getMetricCollection().getValueIntervalCollection();
					RootBusinessLayer.getInstance().getIntervalCollectionBusiness().load(intervalCollection);
					Collection<StudentResultsMetricValue> studentResultsMetricValues = schoolBusinessLayer.getStudentResultsMetricValueBusiness()
							.findByStudentResults(studentClassroomSessionDivision.getResults());
					for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues){
						studentResultsMetricValue.getMetricValue().setNumberValue(new BigDecimal(RandomDataProvider.getInstance().randomInt(intervalCollection.getLowestValue().intValue(), intervalCollection.getHighestValue().intValue())));
						studentResultsMetricValue.getMetricValue().setStringValue(RandomStringUtils.randomAlphabetic(1));
					}
					studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision,studentResultsMetricValues);
				}
			}
		}
	}
	public void randomValues(Boolean metric,Boolean attendance,Boolean appreciation){
		randomValues(schoolBusinessLayer.getClassroomSessionDivisionBusiness().findAll(), metric, attendance, appreciation);
	}
	
	public StudentClassroomSession createStudentClassroomSession(String registrationCode,ClassroomSession classroomSession,Object[][] expected){
		StudentClassroomSession studentClassroomSession = new StudentClassroomSession(studentBusiness.findByRegistrationCode(registrationCode), classroomSession);
		studentClassroomSession = studentClassroomSessionBusiness.create(studentClassroomSession);
		//assertStudentClassroomSession(studentClassroomSession, expected);
		return studentClassroomSession;
	}
	
	public void createStudentClassroomSessions(String[] registrationCodes,ClassroomSession classroomSession,Object[][] expected){
		for(String code : registrationCodes)
			createStudentClassroomSession(code, classroomSession, expected);
	}
	
	public StudentSubject createStudentSubject(String registrationCode,ClassroomSessionDivisionSubject classroomSessionDivisionSubject,Object[][] expected){
		StudentSubject studentSubject = new StudentSubject(studentBusiness.findByRegistrationCode(registrationCode), classroomSessionDivisionSubject);
		studentSubject.setCascadeBottomUpOnCreate(studentSubjectCascadeBottomUpOnCreate);
		studentSubject.setCascadeTopDownOnCreate(studentSubjectCascadeTopDownOnCreate);
		studentSubject = studentSubjectBusiness.create(studentSubject);
		assertStudentClassroomSession(studentClassroomSessionBusiness.findByStudentByClassroomSession(studentSubject.getStudent()
				, studentSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()), expected);
		return studentSubject;
	}
	
	public void assertStudentClassroomSession(StudentClassroomSession studentClassroomSession,Object[][] expected){
		assertEquals("Student classroom session division count", expected.length, 
				schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
		
		int i=0;
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()))
			assertStudentClassroomSessionDivision(studentClassroomSessionDivision, expected[i++]);
		
	}
	
	public void assertStudentClassroomSessionDivision(StudentClassroomSessionDivision studentClassroomSessionDivision,Object[] expected){
		assertEquals("Student classroom session division subject count", (Integer)expected[0], 
				schoolBusinessLayer.getStudentSubjectBusiness()
				.findByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent(), studentClassroomSessionDivision.getClassroomSessionDivision()).size());
	}
	
	public void updateStudentClassroomSession(StudentClassroomSession studentClassroomSession,ClassroomSession classroomSession){
		Integer oldStudentClassroomSessionDivisionCount = schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size();
		
		studentClassroomSession.setClassroomSession(classroomSession);
		
		studentClassroomSessionBusiness.update(studentClassroomSession);
		
		Integer newStudentClassroomSessionDivisionCount = schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness()
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size();
		
		assertEquals("Student classroom session division count", oldStudentClassroomSessionDivisionCount, newStudentClassroomSessionDivisionCount);
	}
	
	public void deleteStudentClassroomSession(StudentClassroomSession studentClassroomSession,Object[][] expected){
		studentClassroomSessionBusiness.delete(studentClassroomSession);
		if(expected==null){
			assertEquals("Student classroom session division count", 0, schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness()
					.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
			
			assertEquals("Student classroom session division subject count", 0, schoolBusinessLayer.getStudentSubjectBusiness()
					.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
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
			studentResultsMetricValue.getMetricValue().setNumberValue(new BigDecimal(values[i++]));
		schoolBusinessLayer.getStudentClassroomSessionDivisionBusiness().update(studentClassroomSessionDivision, new ArrayList<>(studentResultsMetricValues));
		Collection<StudentResultsMetricValue> updateStudentResultsMetricValues = schoolBusinessLayer.getStudentResultsMetricValueBusiness().findByStudentResults(studentClassroomSessionDivision.getResults());
		assertEquals("Student classroom session division metrics count", studentResultsMetricValues.size(), updateStudentResultsMetricValues.size());
		
		for(StudentResultsMetricValue u : studentResultsMetricValues)
			for(StudentResultsMetricValue s : updateStudentResultsMetricValues)
				if(u.getIdentifier().equals(s.getIdentifier())){
					assertEquals("Student results identifier", u.getIdentifier(), s.getIdentifier());
					assertEquals("Student results metric value", u.getMetricValue().getNumberValue(), s.getMetricValue().getNumberValue());
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
	
	public void simulateStudentClassroomSessionDivisionReport(ClassroomSessionDivision classroomSessionDivision,Object[][] objects,Boolean generateReport,Boolean printReport){
		if(objects!=null)
			for(Object[] object : objects){
				createSubjectEvaluations((ClassroomSessionDivisionSubject)object[0],(String[][])object[1]);
			}
    	 
    	if(Boolean.TRUE.equals(generateReport)){
    		randomValues(Arrays.asList(classroomSessionDivision),Boolean.TRUE,Boolean.TRUE,Boolean.TRUE);
    		createStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivision),printReport);
    	}
    }
	
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
    		for(ClassroomSession classroomSession : schoolBusinessLayer.getClassroomSessionBusiness().findAll()){
    			if(levelNames.add(classroomSession.getLevelTimeDivision().getLevel().getName().getCode())){
    				classroomSessions.add(classroomSession);
    			}
    		}
    		createStudentClassroomSessions(parameters.getStudentByClassroomSessionCount(),classroomSessions);
    	}
    	
    	System.out.println(classroomSessions.size()+" classroom session(s) created");
    	
    	System.out.println("Creating subject evaluations");
    	createSubjectEvaluations(Evaluation.COEFFICIENT_APPLIED);
    	
    	System.out.println("Try to create more subject evaluations than allowed");
    	new Try("Vous ne pouvez pas créer plus de 1 evaluation"){ 
			private static final long serialVersionUID = -8176804174113453706L;
			@Override protected void code() {createSubjectEvaluations(Evaluation.COEFFICIENT_APPLIED);}
		}.execute();
    	
    	System.out.println("Setting student metric , attendance , appreciation");
    	randomValues(Boolean.TRUE,Boolean.TRUE,Boolean.TRUE);
    	
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>();
    	for(ClassroomSession classroomSession : classroomSessions)
    		classroomSessionDivisions.addAll(schoolBusinessLayer.getClassroomSessionDivisionBusiness().findByClassroomSession(classroomSession));
    	System.out.println("Creating student classroom session reports");
		createStudentClassroomSessionDivisionReport(classroomSessionDivisions,parameters.getClassroomSessionDivisionIndexes(),Boolean.TRUE);
		
		System.out.println("Creating custom classroom session");
		ClassroomSession customClassroomSession = new ClassroomSession(schoolBusinessLayer.getAcademicSessionBusiness().findCurrent(null)
				, levelTimeDivisionDao.readOneRandomly(), null);
		customClassroomSession.getPeriod().setFromDate(new Date());
		customClassroomSession.getPeriod().setToDate(new Date());
		schoolBusinessLayer.getClassroomSessionBusiness().create(customClassroomSession);
		Collection<ClassroomSessionDivision> customClassroomSessionDivisions = new ArrayList<>();
		ClassroomSessionDivision customClassroomSessionDivision = new ClassroomSessionDivision(customClassroomSession
				, timeDivisionTypeDao.read(TimeDivisionType.TRIMESTER), BigDecimal.ONE);
		customClassroomSessionDivision.getPeriod().setFromDate(new Date());
		customClassroomSessionDivision.getPeriod().setToDate(new Date());
		schoolBusinessLayer.getClassroomSessionDivisionBusiness().create(customClassroomSessionDivision);
		customClassroomSessionDivisions.add(customClassroomSessionDivision);
		Collection<Student> customStudents = schoolBusinessLayer.getStudentBusiness().findManyRandomly(parameters.generatedStudentInClassroomSessionCount);
		Collection<ClassroomSessionDivisionSubject> customClassroomSessionDivisionSubjects = new ArrayList<>();
		for(Subject subject : subjectDao.readManyRandomly(5)){
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(customClassroomSessionDivision, subject
					,BigDecimal.ONE , teacherDao.readOneRandomly());
			customClassroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
			schoolBusinessLayer.getClassroomSessionDivisionSubjectBusiness().create(classroomSessionDivisionSubject);
			for(int i = 0;i<customClassroomSessionDivisionSubjectEvaluationTypeInfos.length;i++){
				EvaluationType evaluationType = (EvaluationType) customClassroomSessionDivisionSubjectEvaluationTypeInfos[i][0];
				BigDecimal coefficient = new BigDecimal((String) customClassroomSessionDivisionSubjectEvaluationTypeInfos[i][1]);
				BigDecimal maximumValue = new BigDecimal((String) customClassroomSessionDivisionSubjectEvaluationTypeInfos[i][2]);
				ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType = new ClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubject, evaluationType, coefficient,maximumValue);
				schoolBusinessLayer.getSubjectEvaluationTypeBusiness().create(subjectEvaluationType);
			}
			for(Student student : customStudents){
				StudentSubject studentSubject = new StudentSubject(student, classroomSessionDivisionSubject);
				studentSubject.setCascadeBottomUpOnCreate(Boolean.TRUE);
				studentSubject.setCascadeTopDownOnCreate(Boolean.FALSE);
				schoolBusinessLayer.getStudentSubjectBusiness().create(studentSubject);
			}
		}
		createSubjectEvaluations(customClassroomSessionDivisionSubjects,Evaluation.COEFFICIENT_APPLIED);
		if(Boolean.TRUE.equals(parameters.getCreateStudentClassroomSessionDivisionReport())){
			System.out.println("Creating student classroom session reports");
			createStudentClassroomSessionDivisionReport(customClassroomSessionDivisions,parameters.getClassroomSessionDivisionIndexes(),Boolean.TRUE);
		}
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
		
		private Boolean createStudentClassroomSessionForAllLevel,createStudentClassroomSessionDivisionReport=Boolean.TRUE;
		private Boolean createFileOnDiskOfOneStudentClassroomSessionDivisionReportForAllLevel;
		
	}
	
}
