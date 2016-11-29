package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MetricBusiness;
import org.cyk.system.root.business.api.mathematics.MetricCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.MetricValueBusiness;
import org.cyk.system.root.business.api.time.AttendanceMetricValueBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessTestHelper;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValue;
import org.cyk.system.root.model.mathematics.MetricValueInputted;
import org.cyk.system.root.model.mathematics.MetricValueType;
import org.cyk.system.root.model.time.AttendanceMetricValue;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.file.FileRepresentationTypeDao;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionTypeDao;
import org.cyk.system.root.persistence.api.time.AttendanceMetricValueDao;
import org.cyk.system.root.persistence.api.time.TimeDivisionTypeDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness.ServiceCallArguments;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.system.school.persistence.api.subject.SubjectDao;
import org.cyk.utility.common.FileExtension;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.cyk.utility.common.test.TestEnvironmentListener.Try;

import lombok.Getter;
import lombok.Setter;

@Singleton
public class SchoolBusinessTestHelper extends AbstractBusinessTestHelper implements Serializable {

	private static final long serialVersionUID = -6893154890151909538L;
	private static SchoolBusinessTestHelper INSTANCE;
	
	@Inject private StudentBusiness studentBusiness;
	@Inject private StudentClassroomSessionDivisionSubjectBusiness studentSubjectBusiness;
	@Inject private StudentClassroomSessionDivisionBusiness studentClassroomSessionDivisionBusiness;
	@Inject private StudentClassroomSessionBusiness studentClassroomSessionBusiness;
	@Inject private EvaluationBusiness subjectEvaluationBusiness;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeBusiness evaluationTypeBusiness;
	
	@Inject private LevelTimeDivisionDao levelTimeDivisionDao;
	@Inject private TimeDivisionTypeDao timeDivisionTypeDao;
	@Inject private SubjectDao subjectDao;
	@Inject private TeacherDao teacherDao;
	
	@Inject private SchoolBusinessLayer schoolBusinessLayer;
	
	@Getter @Setter private Boolean studentSubjectCascadeOperationToMaster,studentSubjectCascadeOperationToChildren;
	
	@Getter @Setter private List<EvaluationType> evaluationTypes = new ArrayList<>();
	@Getter @Setter private Object[][] customClassroomSessionDivisionSubjectEvaluationTypeInfos;
	
	@Getter @Setter private Integer appreciationLenght = 300;
	
	/**/
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
	}
	
	public void randomSetActor(Boolean classCoordinator,Boolean teacher){
		if(Boolean.TRUE.equals(classCoordinator)){
			Collection<ClassroomSession> classroomSessions = inject(ClassroomSessionBusiness.class).findAll();
			for(ClassroomSession classroomSession : classroomSessions)
				classroomSession.setCoordinator(inject(TeacherDao.class).readOneRandomly());
			inject(ClassroomSessionBusiness.class).update(classroomSessions);
		}
		if(Boolean.TRUE.equals(teacher)){
			Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = inject(ClassroomSessionDivisionSubjectBusiness.class).findAll();
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects)
				classroomSessionDivisionSubject.setTeacher(inject(TeacherDao.class).readOneRandomly());
			inject(ClassroomSessionDivisionSubjectBusiness.class).update(classroomSessionDivisionSubjects);
		}
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Integer generateStudentInClassroomSessionCount,Integer studentByClassroomSessionCount){
		Collection<ClassroomSession> classroomSessions = generateStudentInClassroomSessionCount==null?
				inject(ClassroomSessionBusiness.class).findAll():
					inject(ClassroomSessionBusiness.class).findManyRandomly(generateStudentInClassroomSessionCount);
		return createStudentClassroomSessions(inject(StudentBusiness.class).findManyRandomly(studentByClassroomSessionCount),classroomSessions);
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Integer studentByClassroomSessionCount,Collection<ClassroomSession> classroomSessions){
		return createStudentClassroomSessions(inject(StudentBusiness.class).findManyRandomly(studentByClassroomSessionCount),classroomSessions);
	}
	
	public Collection<ClassroomSession> createStudentClassroomSessions(Collection<Student> students,Collection<ClassroomSession> classroomSessions){
		Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		
		for(ClassroomSession classroomSession : classroomSessions)
			for(Student student : students)
				studentClassroomSessions.add(new StudentClassroomSession(student, classroomSession));
				
		inject(StudentClassroomSessionBusiness.class).create(studentClassroomSessions);
		return classroomSessions;
	}
	
	public void deleteStudentClassroomSessions(String[] studentRegistrationCode,ClassroomSessionDivision classroomSessionDivision){
		//inject(StudentClassroomSessionDivisionBusiness.class).del
	}
	
	public void createSubjectEvaluations(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Boolean coefficientApplied){
		Collection<Evaluation> subjectEvaluations = new ArrayList<>();
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
			Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
			Collection<StudentClassroomSessionDivisionSubject> studentSubjects = inject(StudentClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
			if(studentSubjects.isEmpty())
				continue;
			for(ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType : subjectEvaluationTypes){
				Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType);
				subjectEvaluation.setCoefficientApplied(coefficientApplied);
				subjectEvaluations.add(subjectEvaluation);
				for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects ){
					subjectEvaluation.getStudentSubjectEvaluations().add(new StudentClassroomSessionDivisionSubjectEvaluation(subjectEvaluation, studentSubject
							, new BigDecimal(RandomDataProvider.getInstance().randomInt(0, subjectEvaluationType.getMaximumValue().intValue()))));
				}
			}
		}
		System.out.println("Creating "+subjectEvaluations.size()+" evaluation(s)");
		inject(EvaluationBusiness.class).create(subjectEvaluations);
	}
	public void createSubjectEvaluations(Boolean coefficientApplied){
		createSubjectEvaluations(inject(ClassroomSessionDivisionSubjectBusiness.class).findAll(), coefficientApplied);
	}

	public void createStudentClassroomSessionDivisionSubjects(String[] studentRegistrationCodes,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects){
		for(String studentRegistrationCode : studentRegistrationCodes){
			Student student = studentBusiness.find(studentRegistrationCode);
			for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjects){
				StudentClassroomSessionDivisionSubject studentSubject = new StudentClassroomSessionDivisionSubject(student, classroomSessionDivisionSubject);
				studentSubjectBusiness.create(studentSubject);
			}
		}
	}
	
	public void createSubjectEvaluation(ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType,String[][] details){
		Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType); 
		for(String[] detail : details){
			if(StringUtils.isBlank(detail[1]))
				continue;
			Student student = studentBusiness.find(detail[0]);
			StudentClassroomSessionDivisionSubject studentSubject = studentSubjectBusiness.findByStudentByClassroomSessionDivisionSubject(student, subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject());
			subjectEvaluation.getStudentSubjectEvaluations().add(new StudentClassroomSessionDivisionSubjectEvaluation(subjectEvaluation,studentSubject, new BigDecimal(detail[1])));
		}
		//debug(subjectEvaluation);
		//debug(subjectEvaluation.getStudentSubjectEvaluations().iterator().next());
		subjectEvaluationBusiness.create(subjectEvaluation);
	}

	public void createSubjectEvaluation(ClassroomSessionDivisionSubject subject,EvaluationType evaluationType,String[][] details){
		createSubjectEvaluation(evaluationTypeBusiness.findBySubjectByEvaluationType(subject, evaluationType),details);
	}
	
	public void computeStudentClassroomSessionDivisionResults(Collection<ClassroomSessionDivision> classroomSessionDivisions,Set<Integer> classroomSessionDivisionIndexes,Boolean computeEvaluationResults,Boolean computeAttendanceResults,Boolean buildReportFile,Boolean createFileOnDisk){
		/*if(Boolean.TRUE.equals(computeEvaluationResults)){
			System.out.println("Computing evaluation results of "+classroomSessionDivisions.size()+" classroom session divisions");
			studentClassroomSessionDivisionBusiness.updateAverage(classroomSessionDivisions, null);
			
			
			studentClassroomSessionDivisionBusiness.updateRank(classroomSessionDivisions, rankOptions,null);
		}
		if(Boolean.TRUE.equals(computeAttendanceResults)){
			System.out.println("Computing attendance results of "+classroomSessionDivisions.size()+" classroom session divisions");
			studentClassroomSessionDivisionBusiness.updateAttendance(classroomSessionDivisions,null);
		}
		*/
		if(Boolean.TRUE.equals(buildReportFile)){
			System.out.println("Building report of "+classroomSessionDivisions.size()+" classroom session divisions");
			CreateReportFileArguments.Builder<StudentClassroomSessionDivision> reportArgumentsBuilder =  new CreateReportFileArguments.Builder<StudentClassroomSessionDivision>(null)
					.setIsDraft(Boolean.FALSE);
			studentClassroomSessionDivisionBusiness.buildReport(classroomSessionDivisions,computeEvaluationResults,computeAttendanceResults,computeEvaluationResults,schoolBusinessLayer.getStudentEvaluationResultsRankOptions(),reportArgumentsBuilder,new ServiceCallArguments());
			
			if(Boolean.TRUE.equals(createFileOnDisk)){
				Collection<File> files = new ArrayList<>();
				for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
					classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).find(classroomSessionDivision.getIdentifier());
					if(classroomSessionDivisionIndexes==null || classroomSessionDivisionIndexes.isEmpty()
						|| (classroomSessionDivisionIndexes.contains(classroomSessionDivision.getOrderNumber().intValue())) ){
						for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
							studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.find(studentClassroomSessionDivision.getIdentifier());
							if( (studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired() && !inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class)
									.findByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent()
											, studentClassroomSessionDivision.getClassroomSessionDivision()).isEmpty()) || !studentClassroomSessionDivision.getClassroomSessionDivision().getStudentEvaluationRequired()){
								//assertThat("Report of "+studentClassroomSessionDivision.getStudent()+" built", studentClassroomSessionDivision.getResults().getReport()!=null);
								System.out.println("Writing report of : "+studentClassroomSessionDivision.getStudent()+" , "+studentClassroomSessionDivision.getClassroomSessionDivision()+" , "+studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession());
								//writeReport(studentClassroomSessionDivisionBusiness.findReport(studentClassroomSessionDivision));
								write(inject(FileBusiness.class).findByRepresentationTypeByIdentifiable(inject(FileRepresentationTypeDao.class)
										.read(inject(ClassroomSessionBusiness.class).findCommonNodeInformations(studentClassroomSessionDivision.getClassroomSessionDivision()
												.getClassroomSession()).getStudentClassroomSessionDivisionResultsReportTemplate().getCode())
										, studentClassroomSessionDivision).iterator().next());
								//files.add(studentClassroomSessionDivision.getResults().getReport());	
							}
						}
					}
				}
				writeStream(inject(FileBusiness.class).merge(files, FileExtension.PDF), "allreports_"+System.currentTimeMillis(), "pdf");
	    	}
		}
	}
	public void computeStudentClassroomSessionDivisionResults(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean computeEvaluationResults,Boolean computeAttendanceResults,Boolean buildReportFile,Boolean createFileOnDisk){
		computeStudentClassroomSessionDivisionResults(classroomSessionDivisions, null,computeEvaluationResults,computeAttendanceResults,buildReportFile, createFileOnDisk);
	}
	public void computeStudentClassroomSessionDivisionResults(Boolean computeEvaluationResults,Boolean computeAttendanceResults,Boolean buildReportFile,Boolean createFileOnDisk){
		computeStudentClassroomSessionDivisionResults(inject(ClassroomSessionDivisionBusiness.class).findAll(),computeEvaluationResults,computeAttendanceResults,buildReportFile,createFileOnDisk);
	}
	public void createStudentClassroomSessionDivisionReport(ClassroomSessionDivision classroomSessionDivision,Boolean computeEvaluationResults,Boolean computeAttendanceResults,Boolean buildReportFile,Boolean createFileOnDisk){
		computeStudentClassroomSessionDivisionResults(Arrays.asList(classroomSessionDivision),computeEvaluationResults,computeAttendanceResults,buildReportFile, createFileOnDisk);
	}
	
	public void randomValues(Collection<ClassroomSessionDivision> classroomSessionDivisions,Boolean metric,Boolean attendance,Boolean appreciation){
		Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = new ArrayList<>();
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions){
			Long t = inject(ClassroomSessionBusiness.class).findCommonNodeInformations(classroomSessionDivision.getClassroomSession()).getAttendanceTimeDivisionType().getDuration();
			for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisionBusiness.findByClassroomSessionDivision(classroomSessionDivision)){
				studentClassroomSessionDivisions.add(studentClassroomSessionDivision);
				
				if(Boolean.TRUE.equals(attendance)){
					Collection<AttendanceMetricValue> attendanceMetricValues = inject(AttendanceMetricValueDao.class).readByAttendanceByCodes(
							studentClassroomSessionDivision.getResults().getLectureAttendance(),AttendanceMetricValue.NUMBER_OF_MILLISECOND_ATTENDED
							,AttendanceMetricValue.NUMBER_OF_MILLISECOND_MISSED);
					inject(AttendanceMetricValueBusiness.class).setValue(attendanceMetricValues, AttendanceMetricValue.NUMBER_OF_MILLISECOND_ATTENDED, new BigDecimal(randomDataProvider.randomInt(0, 1)*t));
					inject(AttendanceMetricValueBusiness.class).setValue(attendanceMetricValues, AttendanceMetricValue.NUMBER_OF_MILLISECOND_MISSED, new BigDecimal(randomDataProvider.randomInt(0, 1)*t));
					
					//studentClassroomSessionDivision = studentClassroomSessionDivisionBusiness.update(studentClassroomSessionDivision);
				}
				/*
				if(Boolean.TRUE.equals(metric)){
					Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = 
							classroomSessionDivisionStudentsMetricCollectionDao.readByClassroomSessionDivision(classroomSessionDivision);
					for(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection : classroomSessionDivisionStudentsMetricCollections){
						IntervalCollection intervalCollection = classroomSessionDivisionStudentsMetricCollection.getMetricCollection().getValueIntervalCollection();
						inject(IntervalCollectionBusiness.class).load(intervalCollection);
						Collection<StudentResultsMetricValue> studentResultsMetricValues = inject(StudentResultsMetricValueBusiness.class)
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
			inject(MetricValueBusiness.class).updateManyRandomly(new LinkedHashSet<String>(Arrays.asList(SchoolConstant.Code.MetricCollectionType.STUDENT_BEHAVIOUR
					,SchoolConstant.Code.MetricCollectionType.STUDENT_ATTENDANCE)), classroomSessionDivisions, studentClassroomSessionDivisions);
			/*for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
				Collection<MetricCollection> metricCollections = inject(MetricCollectionBusiness.class).findByTypesByIdentifiable(inject(MetricCollectionTypeDao.class)
						.read(Arrays.asList(SchoolConstant.Code.MetricCollectionType.STUDENT_BEHAVIOUR,SchoolConstant.Code.MetricCollectionType.STUDENT_ATTENDANCE))
						, studentClassroomSessionDivision.getClassroomSessionDivision());
				for(MetricCollection metricCollection : metricCollections){
					Collection<Metric> metrics = inject(MetricBusiness.class).findByCollection(metricCollection);
					IntervalCollection intervalCollection = metricCollection.getValueIntervalCollection();
					inject(IntervalCollectionBusiness.class).load(intervalCollection);
					List<Interval> intervals = new ArrayList<>(inject(IntervalBusiness.class).findByCollection(intervalCollection));
					Collection<MetricValue> metricValues = inject(MetricValueBusiness.class).findByMetricsByIdentifiables(metrics,Arrays.asList(studentClassroomSessionDivision));
					Collection<AbstractIdentifiable> c = new ArrayList<>();
					for(MetricValue metricValue : metricValues){
						if(MetricValueType.NUMBER.equals(metricCollection.getValueType())){
							metricValue.getNumberValue().setUser(new BigDecimal(RandomDataProvider.getInstance().randomInt(intervalCollection.getLowestValue().intValue(), intervalCollection.getHighestValue().intValue())));
						}else
							if(MetricValueInputted.VALUE_INTERVAL_CODE.equals(metricValue.getMetric().getCollection().getValueInputted()))
								metricValue.setStringValue( ((Interval)RandomDataProvider.getInstance().randomFromList(intervals)).getCode() );
							else
								metricValue.setStringValue(RandomStringUtils.randomAlphabetic(1));
						c.add(metricValue);
					}
					inject(GenericBusiness.class).update(c);
				}
			}*/
		}
	}
	public void randomValues(Boolean metric,Boolean attendance,Boolean appreciation){
		randomValues(inject(ClassroomSessionDivisionBusiness.class).findAll(), metric, attendance, appreciation);
	}
	
	public StudentClassroomSession createStudentClassroomSession(String registrationCode,ClassroomSession classroomSession,Object[][] expected){
		StudentClassroomSession studentClassroomSession = new StudentClassroomSession(studentBusiness.find(registrationCode), classroomSession);
		studentClassroomSession = studentClassroomSessionBusiness.create(studentClassroomSession);
		//assertStudentClassroomSession(studentClassroomSession, expected);
		return studentClassroomSession;
	}
	
	public void createStudentClassroomSessions(String[] registrationCodes,ClassroomSession classroomSession,Object[][] expected){
		for(String code : registrationCodes)
			createStudentClassroomSession(code, classroomSession, expected);
	}
	
	public StudentClassroomSessionDivisionSubject createStudentSubject(String registrationCode,ClassroomSessionDivisionSubject classroomSessionDivisionSubject,Object[][] expected){
		StudentClassroomSessionDivisionSubject studentSubject = new StudentClassroomSessionDivisionSubject(studentBusiness.find(registrationCode), classroomSessionDivisionSubject);
		studentSubject.setCascadeOperationToMaster(studentSubjectCascadeOperationToMaster);
		studentSubject.setCascadeOperationToChildren(studentSubjectCascadeOperationToChildren);
		studentSubject = studentSubjectBusiness.create(studentSubject);
		assertStudentClassroomSession(studentClassroomSessionBusiness.findByStudentByClassroomSession(studentSubject.getStudent()
				, studentSubject.getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()), expected);
		return studentSubject;
	}
	
	public void assertStudentClassroomSession(StudentClassroomSession studentClassroomSession,Object[][] expected){
		assertEquals("Student classroom session division count", expected.length, 
				inject(StudentClassroomSessionDivisionBusiness.class)
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
		
		int i=0;
		for(StudentClassroomSessionDivision studentClassroomSessionDivision : inject(StudentClassroomSessionDivisionBusiness.class)
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()))
			assertStudentClassroomSessionDivision(studentClassroomSessionDivision, expected[i++]);
		
	}
	
	public void assertStudentClassroomSessionDivision(StudentClassroomSessionDivision studentClassroomSessionDivision,Object[] expected){
		assertEquals("Student classroom session division subject count", (Integer)expected[0], 
				inject(StudentClassroomSessionDivisionSubjectBusiness.class)
				.findByStudentByClassroomSessionDivision(studentClassroomSessionDivision.getStudent(), studentClassroomSessionDivision.getClassroomSessionDivision()).size());
	}
	
	public void updateStudentClassroomSession(StudentClassroomSession studentClassroomSession,ClassroomSession classroomSession){
		Integer oldStudentClassroomSessionDivisionCount = inject(StudentClassroomSessionDivisionBusiness.class)
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size();
		
		studentClassroomSession.setClassroomSession(classroomSession);
		
		studentClassroomSessionBusiness.update(studentClassroomSession);
		
		Integer newStudentClassroomSessionDivisionCount = inject(StudentClassroomSessionDivisionBusiness.class)
				.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size();
		
		assertEquals("Student classroom session division count", oldStudentClassroomSessionDivisionCount, newStudentClassroomSessionDivisionCount);
	}
	
	public void deleteStudentClassroomSession(StudentClassroomSession studentClassroomSession,Object[][] expected){
		studentClassroomSessionBusiness.delete(studentClassroomSession);
		if(expected==null){
			assertEquals("Student classroom session division count", 0, inject(StudentClassroomSessionDivisionBusiness.class)
					.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
			
			assertEquals("Student classroom session division subject count", 0, inject(StudentClassroomSessionDivisionSubjectBusiness.class)
					.findByStudentByClassroomSession(studentClassroomSession.getStudent(), studentClassroomSession.getClassroomSession()).size());
		}else{
			assertStudentClassroomSession(studentClassroomSession, expected);
		}
		
	}
	
	public void deleteStudentClassroomSession(StudentClassroomSession studentClassroomSession){
		deleteStudentClassroomSession(studentClassroomSession, null);
	}
	
	@Deprecated
	public void updateStudentClassroomSessionDivision(StudentClassroomSessionDivision studentClassroomSessionDivision,Collection<StudentResultsMetricValue> studentResultsMetricValues,String[] values) {
		int i = 0;
		for(StudentResultsMetricValue studentResultsMetricValue : studentResultsMetricValues)
			studentResultsMetricValue.getMetricValue().getNumberValue().setUser(new BigDecimal(values[i++]));
		inject(StudentClassroomSessionDivisionBusiness.class).update(studentClassroomSessionDivision, new ArrayList<>(studentResultsMetricValues));
		Collection<StudentResultsMetricValue> updateStudentResultsMetricValues = inject(StudentResultsMetricValueBusiness.class).findByStudentResults(studentClassroomSessionDivision.getResults());
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
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectBusiness.updateAverage(Arrays.asList(classroomSessionDivisionSubject), null);
		for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
			for(String[] detail : details)
				if(detail[0].equals(studentSubject.getStudent().getCode())){
					assertBigDecimalEquals("Average of "+studentSubject.getStudent(), detail[1], studentSubject.getResults().getEvaluationSort().getAverage().getValue());
				}
		}
	}
	
	public void assertClassroomSessionDivisionSubjectRank(ClassroomSessionDivisionSubject classroomSessionDivisionSubject,String[][] details,RankOptions<SortableStudentResults> rankOptions){
		Collection<StudentClassroomSessionDivisionSubject> studentSubjects = studentSubjectBusiness.updateAverage(Arrays.asList(classroomSessionDivisionSubject), null);
		studentSubjectBusiness.rank(studentSubjects,rankOptions,null);
		for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
			for(String[] detail : details)
				if(detail[0].equals(studentSubject.getStudent().getCode())){
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
				assertClassroomSessionDivisionSubjectRank(classroomSessionDivisionSubject,extract(details, index),schoolBusinessLayer.getStudentEvaluationResultsRankOptions());
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
	
	public void simulateStudentClassroomSessionDivisionReport(ClassroomSessionDivision classroomSessionDivision,Object[][] objects,Boolean computeEvaluationResults,Boolean computeAttendanceResults,Boolean generateReport,Boolean printReport,Boolean email){
		if(objects!=null)
			for(Object[] object : objects){
				createSubjectEvaluations((ClassroomSessionDivisionSubject)object[0],(String[][])object[1]);
			}
    	 
    	if(Boolean.TRUE.equals(computeEvaluationResults)){
    		randomValues(Arrays.asList(classroomSessionDivision),Boolean.TRUE,Boolean.TRUE,Boolean.TRUE);
    		//createStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivision),computeEvaluationResults,computeAttendanceResults,generateReport,printReport);
    	}
    	
    	computeStudentClassroomSessionDivisionResults(Arrays.asList(classroomSessionDivision),computeEvaluationResults,computeAttendanceResults,generateReport,printReport);
    	
    	if(Boolean.TRUE.equals(email))
    		inject(StudentClassroomSessionDivisionBusiness.class).sendReportFileToEmail(inject(StudentClassroomSessionDivisionBusiness.class)
				.findByClassroomSessionDivisions(Arrays.asList(classroomSessionDivision)));
	}
	
	public void simulate(SchoolBusinessSimulationParameters parameters){
		System.out.println("School business simulation started");
		
    	System.out.println("Creating teachers");
    	inject(TeacherBusiness.class).create(inject(TeacherBusiness.class).instanciateManyRandomly(parameters.getTeacherCount()));
		System.out.println("Creating students");
		inject(StudentBusiness.class).create(inject(StudentBusiness.class).instanciateManyRandomly(parameters.getStudentCount()));
    	
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
    		for(ClassroomSession classroomSession : inject(ClassroomSessionBusiness.class).findAll()){
    			if(levelNames.add(classroomSession.getLevelTimeDivision().getLevel().getLevelName().getCode())){
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
    		classroomSessionDivisions.addAll(inject(ClassroomSessionDivisionBusiness.class).findByClassroomSession(classroomSession));
    	System.out.println("Creating student classroom session reports");
		computeStudentClassroomSessionDivisionResults(classroomSessionDivisions,parameters.getClassroomSessionDivisionIndexes(),parameters.getComputeEvaluationResults(),parameters.getComputeAttendanceResults(),parameters.getBuildReportFile(),Boolean.TRUE);
		
		System.out.println("Creating custom classroom session");
		ClassroomSession customClassroomSession = new ClassroomSession(inject(AcademicSessionBusiness.class).findCurrent(null)
				, levelTimeDivisionDao.readOneRandomly(), null);
		customClassroomSession.getExistencePeriod().setFromDate(new Date());
		customClassroomSession.getExistencePeriod().setToDate(new Date());
		inject(ClassroomSessionBusiness.class).create(customClassroomSession);
		Collection<ClassroomSessionDivision> customClassroomSessionDivisions = new ArrayList<>();
		ClassroomSessionDivision customClassroomSessionDivision = new ClassroomSessionDivision(customClassroomSession
				, timeDivisionTypeDao.read(TimeDivisionType.TRIMESTER), BigDecimal.ONE);
		customClassroomSessionDivision.getExistencePeriod().setFromDate(new Date());
		customClassroomSessionDivision.getExistencePeriod().setToDate(new Date());
		inject(ClassroomSessionDivisionBusiness.class).create(customClassroomSessionDivision);
		customClassroomSessionDivisions.add(customClassroomSessionDivision);
		Collection<Student> customStudents = inject(StudentBusiness.class).findManyRandomly(parameters.generatedStudentInClassroomSessionCount);
		Collection<ClassroomSessionDivisionSubject> customClassroomSessionDivisionSubjects = new ArrayList<>();
		for(Subject subject : subjectDao.readManyRandomly(5)){
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(customClassroomSessionDivision, subject
					,BigDecimal.ONE , teacherDao.readOneRandomly());
			customClassroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
			inject(ClassroomSessionDivisionSubjectBusiness.class).create(classroomSessionDivisionSubject);
			for(int i = 0;i<customClassroomSessionDivisionSubjectEvaluationTypeInfos.length;i++){
				EvaluationType evaluationType = (EvaluationType) customClassroomSessionDivisionSubjectEvaluationTypeInfos[i][0];
				BigDecimal coefficient = new BigDecimal((String) customClassroomSessionDivisionSubjectEvaluationTypeInfos[i][1]);
				BigDecimal maximumValue = new BigDecimal((String) customClassroomSessionDivisionSubjectEvaluationTypeInfos[i][2]);
				ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType = new ClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubject, evaluationType, coefficient,maximumValue);
				inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).create(subjectEvaluationType);
			}
			for(Student student : customStudents){
				StudentClassroomSessionDivisionSubject studentSubject = new StudentClassroomSessionDivisionSubject(student, classroomSessionDivisionSubject);
				studentSubject.setCascadeOperationToMaster(Boolean.TRUE);
				studentSubject.setCascadeOperationToChildren(Boolean.FALSE);
				inject(StudentClassroomSessionDivisionSubjectBusiness.class).create(studentSubject);
			}
		}
		createSubjectEvaluations(customClassroomSessionDivisionSubjects,Evaluation.COEFFICIENT_APPLIED);
		if(Boolean.TRUE.equals(parameters.getCreateStudentClassroomSessionDivisionReport())){
			System.out.println("Creating student classroom session reports");
			computeStudentClassroomSessionDivisionResults(customClassroomSessionDivisions,parameters.getClassroomSessionDivisionIndexes()
					,parameters.getComputeEvaluationResults(),parameters.getComputeAttendanceResults(),parameters.getBuildReportFile(),Boolean.TRUE);
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
		private Boolean computeEvaluationResults,computeAttendanceResults,buildReportFile;
		
	}
	
	
}
