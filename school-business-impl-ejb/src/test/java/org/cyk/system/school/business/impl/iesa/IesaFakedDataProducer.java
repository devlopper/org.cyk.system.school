package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.cyk.system.company.business.api.structure.CompanyBusiness;
import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.CompanyBusinessLayerAdapter;
import org.cyk.system.company.model.structure.Company;
import org.cyk.system.root.business.impl.party.ApplicationBusinessImpl;
import org.cyk.system.root.business.impl.party.ApplicationBusinessImplListener;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.event.EventMissed;
import org.cyk.system.root.model.event.EventMissedReason;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.security.Installation;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionDivisionInfos;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionDivisionSubjectInfos;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionInfos;
import org.cyk.system.school.business.impl.integration.AbstractSchoolFakedDataProducer;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.joda.time.DateTimeConstants;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractSchoolFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	@Inject private CompanyBusiness companyBusiness;
	@Inject private CompanyBusinessLayer companyBusinessLayer;
	@Inject private EvaluationBusiness subjectEvaluationBusiness;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeBusiness subjectEvaluationTypeBusiness;
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentBusiness studentBusiness;
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private ClassroomSessionDivisionSubjectBusiness classroomSessionDivisionSubjectBusiness;
	@Inject private LectureBusiness lectureBusiness;
	@Inject private TeacherDao teacherDao;
	@Inject private PersonDao personDao;
	
	private Subject subjectNameEnglishLanguage,subjectNameLiteratureInEnglish,subjectNameHistory,subjectNameGeography
		,subjectNameSocialStudies,subjectNameReligiousStudies,subjectNameMathematics,subjectNamePhysics,subjectNameChemistry,subjectNameBiology,subjectNameFrench
		,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation,subjectNameGrammar,subjectNameReadingComprehension,subjectNameHandWriting,
		subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameScience;
	private EvaluationType evaluationTypeTest1,evaluationTypeTest2,evaluationTypeExam;
	private Interval intervalGradingScaleAStar,intervalGradingScaleA,intervalGradingScaleB,intervalGradingScaleC,intervalGradingScaleD,intervalGradingScaleE;
	private Interval intervalEffortLevel1,intervalEffortLevel2,intervalEffortLevel3,intervalEffortLevel4,intervalEffortLevel5;
	private LevelName levelNameG1,levelNameG2,levelNameG3;
	private Level levelG1,levelG2,levelG3;
	private LevelTimeDivision levelTimeDivisionG1,levelTimeDivisionG2,levelTimeDivisionG3;
	private ClassroomSession classroomSessionG1,classroomSessionG2,classroomSessionG3;
	private ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3; 
	private ClassroomSessionDivisionSubject subjectEnglishLanguage,subjectLiteratureInEnglish,subjectHistory,subjectGeography
	,subjectSocialStudies,subjectReligiousStudies,subjectMathematics,subjectPhysics,subjectChemistry,subjectBiology,subjectFrench
	,subjectArtAndCraft,subjectMusic,subjectICT,subjectPhysicalEducation,subjectGrammar,subjectReadingComprehension,subjectHandWriting,
	subjectSpelling,subjectPhonics,subjectCreativeWriting,subjectMoralEducation,subjectScience;
	
	private ClassroomSessionInfos grade1,grade2,grade3;
	
	private Collection<ClassroomSessionDivisionSubject> grade1Subjects = new ArrayList<>();
	private Collection<EvaluationType> evaluationTypes = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>();
	
	private MetricCollection studentWorkMetricCollection;
	
	private CommonNodeInformations commonNodeInformations;
	
	@Setter private Integer numbreOfTeachers = 10;
	@Setter private Integer numbreOfStudents = 10;
	@Setter private Integer numbreOfLecturesByClassroomSessionDivisionSubject = 5;
	@Setter private Integer numbreOfStudentsByClassroomSession = 25;
	
	@Setter private Boolean generateCompleteAcademicSession = Boolean.FALSE;
	@Setter private Boolean generateStudentClassroomSessionDivisionReport = Boolean.FALSE;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		ApplicationBusinessImpl.LISTENERS.add(new ApplicationBusinessImplListener.Adapter.Default(){
			private static final long serialVersionUID = 6894726061444433277L;

			@Override
			public void installationStarted(Installation installation) {
				super.installationStarted(installation);
				installation.getApplication().setName("IESA WebApp");
			}
		});
		
		companyBusinessLayer.getCompanyBusinessLayerListeners().add(new CompanyBusinessLayerAdapter() {
			private static final long serialVersionUID = 5179809445850168706L;

			@Override
			public String getCompanyName() {
				return "IESA";
			}
			
			@Override
			public byte[] getCompanyLogoBytes() {
				return getResourceAsBytes(SchoolBusinessLayer.class.getPackage(),"image/iesa.png");
			}
			
			@Override
			public void handleCompanyToInstall(Company company) {
				super.handleCompanyToInstall(company);
				addContacts(company.getContactCollection(), new String[]{"RueJ7 1-II Plateux Vallon, Cocody"}, new String[]{"22417217","21014459"}
				, new String[]{"05996283","49925138","06173731"}, new String[]{"08 BP 1828 Abidjan 08"}, new String[]{"iesa@aviso.ci"}, new String[]{"http://www.iesaci.com"});
			}

		});
	}
	
	@Override
	protected void structure(){
		// Subjects
		subjectNameEnglishLanguage = createEnumeration(Subject.class,"English Language");
		subjectNameGrammar = createEnumeration(Subject.class,"Grammar");
		subjectNameReadingComprehension = createEnumeration(Subject.class,"Reading & Comprehension");
		subjectNameHandWriting = createEnumeration(Subject.class,"Hand writing");
		subjectNameSpelling = createEnumeration(Subject.class,"Spelling");
		subjectNamePhonics = createEnumeration(Subject.class,"Phonics");
		subjectNameCreativeWriting = createEnumeration(Subject.class,"Creative writing");
		subjectNameMoralEducation = createEnumeration(Subject.class,"Moral education");
		subjectNameScience = createEnumeration(Subject.class,"Science");
		subjectNameLiteratureInEnglish = createEnumeration(Subject.class,"Literature in english");
		subjectNameHistory = createEnumeration(Subject.class,"History");
		subjectNameGeography = createEnumeration(Subject.class,"Geography");
		subjectNameSocialStudies = createEnumeration(Subject.class,"Social Studies");
		subjectNameReligiousStudies = createEnumeration(Subject.class,"Religious studies/Divinity");
		subjectNameMathematics = createEnumeration(Subject.class,"Mathematics");
		subjectNamePhysics = createEnumeration(Subject.class,"Physics");
		subjectNameChemistry = createEnumeration(Subject.class,"Chemistry");
		subjectNameBiology = createEnumeration(Subject.class,"Biology");
		subjectNameFrench = createEnumeration(Subject.class,"French");
		subjectNameArtAndCraft = createEnumeration(Subject.class,"Art & Craft");
		subjectNameMusic = createEnumeration(Subject.class,"Music");
		subjectNameICT = createEnumeration(Subject.class,"ICT");
		subjectNamePhysicalEducation = createEnumeration(Subject.class,"Physical education");
		//Evaluation Type
		evaluationTypes.add(evaluationTypeTest1 = createEnumeration(EvaluationType.class,"Test 1"));
		evaluationTypes.add(evaluationTypeTest2 = createEnumeration(EvaluationType.class,"Test 2"));
		evaluationTypes.add(evaluationTypeExam = createEnumeration(EvaluationType.class,"Exam"));
		
		//Grades
		
		IntervalCollection intervalCollection = createIntervalCollection("ICEV1",new String[][]{
			{"A*", "Outstanding", "90", "100"},{"A", "Excellent", "80", "89.99"},{"B", "Very Good", "70", "79.99"},{"C", "Good", "60", "69.99"}
			,{"D", "Satisfactory", "50", "59.99"},{"E", "Fail", "0", "49.99"}
		});
		
		studentWorkMetricCollection = new MetricCollection("BSWH","Behaviour,Study and Work Habits");
		studentWorkMetricCollection.addItem("1","Respect authority");
		studentWorkMetricCollection.addItem("2","Works independently and neatly");
		studentWorkMetricCollection.addItem("3","Completes homework and class work on time");
		studentWorkMetricCollection.addItem("4","Shows social courtesies");
		studentWorkMetricCollection.addItem("5","Demonstrates self-control");
		studentWorkMetricCollection.addItem("6","Takes care of school and others materials");
		studentWorkMetricCollection.addItem("7","Game/Sport");
		studentWorkMetricCollection.addItem("8","Handwriting");
		studentWorkMetricCollection.addItem("9","Drawing/Painting");
		studentWorkMetricCollection.addItem("10","Punctionality/Regularity");
		studentWorkMetricCollection.addItem("11","Works cooperatively in groups");
		studentWorkMetricCollection.addItem("12","Listens and follows directions");
		
		studentWorkMetricCollection.setValueIntervalCollection(new IntervalCollection("BSWH_METRIC_IC"));
		//studentWorkMetricCollection.getValueIntervalCollection().setLowestValue(new BigDecimal("1"));
		//studentWorkMetricCollection.getValueIntervalCollection().setHighestValue(new BigDecimal("5"));
		studentWorkMetricCollection.getValueIntervalCollection().addItem("1", "Has no regard for the observable traits", "1", "1");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("2", "Shows minimal regard for the observable traits", "2", "2");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("3", "Acceptable level of observable traits", "3", "3");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("4", "Maintains high level of observable traits", "4", "4");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("5", "Maintains an excellent degree of observable traits", "5", "5");
		
		create(studentWorkMetricCollection);
		ReportTemplate reportTemplate = new ReportTemplate("SCSDRT",createFile("report/iesa.jrxml", "reportcard.jrxml"),null);
		create(reportTemplate);
		commonNodeInformations = new CommonNodeInformations(intervalCollection,studentWorkMetricCollection,reportTemplate,getEnumeration(TimeDivisionType.class, TimeDivisionType.DAY));
		commonNodeInformations.setClassroomSessionTimeDivisionType(getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER));
		commonNodeInformations.setCurrentClassroomSessionDivisionIndex(new Byte("1"));
		
		//Level names
		levelNameG1 = createLevelName("Grade 1");
		levelNameG2 = createLevelName("Grade 2");
		levelNameG3 = createLevelName("Grade 3");
		
		levelG1 = create(new Level(null,levelNameG1, null));
		levelG2 = create(new Level(null,levelNameG2, null));
		levelG3 = create(new Level(null,levelNameG3, null));
		
		levelTimeDivisionG1 = create(new LevelTimeDivision(levelG1, getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR)));
		levelTimeDivisionG2 = create(new LevelTimeDivision(levelG2, getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR)));
		levelTimeDivisionG3 = create(new LevelTimeDivision(levelG3, getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR)));
		flush("Before actors");
		
		rootRandomDataProvider.createActor(Teacher.class, numbreOfTeachers);
		flush("Teachers");
		rootRandomDataProvider.createActor(Student.class, numbreOfStudents);
		flush("Students");
		
		School school = new School(ownedCompanyBusiness.findDefaultOwnedCompany(),commonNodeInformations);
    	create(school);
    	
    	school.getOwnedCompany().getCompany().setManager(personDao.readOneRandomly());
    	companyBusiness.update(school.getOwnedCompany().getCompany());
    	
    	AcademicSession academicSession = new AcademicSession(school,commonNodeInformations,new Date());
    	academicSession.getPeriod().setFromDate(new Date());
    	academicSession.getPeriod().setToDate(new Date());
    	academicSession = create(academicSession);
    	flush("Academic session");
    	
    	Collection<ClassroomSession> classroomSessions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>();
    	Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>(); 
    	
    	grade1 = grade(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession, levelTimeDivisionG1,new Subject[]{
    			subjectNameMathematics,subjectNameGrammar,subjectNameReadingComprehension
    			,subjectNameHandWriting,subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameSocialStudies
    			,subjectNameScience,subjectNameFrench,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation},listener);
    	
    	grade2 = grade(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession, levelTimeDivisionG2,new Subject[]{subjectNameMathematics,subjectNameGrammar,subjectNameReadingComprehension
    			,subjectNameHandWriting,subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameSocialStudies
    			,subjectNameScience,subjectNameFrench,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation},listener);
    	
    	grade3 = grade(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession, levelTimeDivisionG3,new Subject[]{subjectNameMathematics,subjectNameGrammar,subjectNameReadingComprehension
    			,subjectNameHandWriting,subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameSocialStudies
    			,subjectNameScience,subjectNameFrench,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation},listener);
    	
    	flush(ClassroomSession.class, classroomSessionBusiness, classroomSessions);
    	flush(ClassroomSessionDivision.class, classroomSessionDivisionBusiness, classroomSessionDivisions);
    	flush(ClassroomSessionDivisionSubject.class, classroomSessionDivisionSubjectBusiness, classroomSessionDivisionSubjects);
    	flush(ClassroomSessionDivisionSubjectEvaluationType.class, subjectEvaluationTypeBusiness, subjectEvaluationTypes);
	}
	
	@Override
	public void produce(FakedDataProducerListener listener) {
		this.listener =listener;
		rootDataProducerHelper.setBasePackage(SchoolBusinessLayer.class.getPackage());
		SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.getEvaluationTypeCodes().addAll(Arrays.asList("Test1","Test2","Exam"));
    	SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.setSumMarks(Boolean.TRUE);
		//schoolBusinessLayer.setAverageComputationListener(new Averagec);
		
    	structure();
    	
    	if(Boolean.TRUE.equals(generateCompleteAcademicSession)){
    		doBusiness(listener);
    	}
	}
	
	@Override
	protected void doBusiness(FakedDataProducerListener listener){
		//ExecutorService executor = Executors.newFixedThreadPool(5);
		//Collection<StudentSubject> studentSubjects = new ArrayList<>();
		for(ClassroomSessionInfos classroomSessionInfos : new ClassroomSessionInfos[]{grade1,grade2,grade3}){
			Collection<Student> students = studentBusiness.findManyRandomly(numbreOfStudentsByClassroomSession);
			createStudentClassroomSessions(classroomSessionInfos, students);	
			//executor.execute(new ClassroomsessionBusinessProducer(classroomSessionInfos, listener, students,studentSubjects));
		}
		//executor.shutdown();
        //while (!executor.isTerminated()) {}
		
		//flush(StudentSubject.class,studentSubjectBusiness,studentSubjects);
		
		Collection<Evaluation> subjectEvaluations = new ArrayList<>();
		for(ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType : subjectEvaluationTypeBusiness.findAll()){
			Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType, Boolean.FALSE);
			for(StudentSubject studentSubject :studentSubjectDao.readByClassroomSessionDivisionSubject(subjectEvaluationType.getClassroomSessionDivisionSubject()) ){
				StudentSubjectEvaluation studentSubjectEvaluation = new StudentSubjectEvaluation(subjectEvaluation, studentSubject
						, new BigDecimal(RandomDataProvider.getInstance().randomInt(0, subjectEvaluationType.getMaximumValue().intValue())));
				subjectEvaluation.getStudentSubjectEvaluations().add(studentSubjectEvaluation);
			}
			subjectEvaluations.add(subjectEvaluation);
			flush(Evaluation.class,subjectEvaluationBusiness,subjectEvaluations,10000l);
		}
		flush(Evaluation.class,subjectEvaluationBusiness,subjectEvaluations);
		
		Collection<Lecture> lectures = new ArrayList<>();
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjectBusiness.findAll()){
			for(int i=0;i<numbreOfLecturesByClassroomSessionDivisionSubject;i++){
				Event event = new Event();
				event.getPeriod().setFromDate(new Date());
				event.getPeriod().setToDate(new Date());
				Lecture lecture = new Lecture(classroomSessionDivisionSubject, event);
				lectures.add(lecture);
				for(StudentSubject studentSubject : studentSubjectBusiness.findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject)){
					EventParticipation eventParticipation = new EventParticipation(studentSubject.getStudent().getPerson());
					if(RandomDataProvider.getInstance().randomInt(1, 5)==3){
						EventMissed eventMissed = new EventMissed(eventParticipation, rootRandomDataProvider.oneFromDatabase(EventMissedReason.class),DateUtils.MILLIS_PER_DAY);
						eventParticipation.setMissed(eventMissed);
					}
					event.getEventParticipations().add(eventParticipation);
				}
				flush(Lecture.class,lectureBusiness,lectures,10000l);
			}
		}
		flush(Lecture.class,lectureBusiness,lectures);
	
		if(Boolean.TRUE.equals(generateStudentClassroomSessionDivisionReport)){
			System.out.println("Updating metric value");
			ClassroomSessionInfos classroomSessionInfos = grade1;
			SchoolBusinessTestHelper.getInstance().randomValues(Arrays.asList(classroomSessionInfos.division(0).getClassroomSessionDivision()), Boolean.TRUE, Boolean.TRUE,Boolean.TRUE);
			
			System.out.println("Generating report");
			SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().buildReport(Arrays.asList(classroomSessionInfos.division(0).getClassroomSessionDivision()));
		}
	}

	private void createStudentClassroomSessions(ClassroomSessionInfos classroomSessionInfos,Collection<Student> students){
		System.out.println("Creating data of classroom session "+classroomSessionInfos.getClassroomSession().getIdentifier()+" with "+students.size()+" students");
		for(Student student : students){
			SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().create(new StudentClassroomSession(student, classroomSessionInfos.getClassroomSession()));	
		}
	}
	
	/**/
	
	private LevelName createLevelName(String name){
		LevelName levelName = new LevelName();
		levelName.setCode(StringUtils.replace(name, Constant.CHARACTER_SPACE.toString(), Constant.EMPTY_STRING));
		levelName.setName(name);
		levelName.setNodeInformations(commonNodeInformations);
		return create(levelName);
	}
	
	private ClassroomSessionInfos grade(Collection<ClassroomSession> classroomSessions,Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes,AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Subject[] subjects,FakedDataProducerListener listener){
		ClassroomSession classroomSession = new ClassroomSession(academicSession, levelTimeDivision, teacherDao.readOneRandomly());
		classroomSession.setStudentClassroomSessionDivisionRankable(levelTimeDivision.getLevel().getName().getCode().equals("Grade1"));
		classroomSession.getPeriod().setFromDate(new Date());
		classroomSession.getPeriod().setToDate(new Date());
		classroomSessions.add(classroomSession);
		ClassroomSessionInfos classroomSessionInfos = new ClassroomSessionInfos(classroomSession);
		classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionInfos.getClassroomSession(),subjects));
		classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionInfos.getClassroomSession(),subjects));
		classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionInfos.getClassroomSession(),subjects));
		return classroomSessionInfos;
	}
	
	private ClassroomSessionDivisionInfos createClassroomSessionDivision(Collection<ClassroomSessionDivision> classroomSessionDivisions,Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes,ClassroomSession classroomSession,Subject[] subjects){
		ClassroomSessionDivision classroomSessionDivision = new ClassroomSessionDivision(classroomSession,getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER)
    			,new BigDecimal("1"));
		classroomSessionDivision.setDuration(DateTimeConstants.MILLIS_PER_DAY * 45l);
		classroomSessionDivisions.add(classroomSessionDivision);
		classroomSessionDivision.getPeriod().setFromDate(new Date());
		classroomSessionDivision.getPeriod().setToDate(new Date());
		ClassroomSessionDivisionInfos classroomSessionDivisionInfos = new ClassroomSessionDivisionInfos(classroomSessionDivision);
		
		for(Subject subject : subjects){
			classroomSessionDivisionInfos.getSubjects().add(createClassroomSessionDivisionSubject(classroomSessionDivisionSubjects,subjectEvaluationTypes,classroomSessionDivision,subject,new Object[][]{
				{evaluationTypeTest1,"0.15"},{evaluationTypeTest2,"0.15"},{evaluationTypeExam,"0.7"}
			}));
    	}
    	
		return classroomSessionDivisionInfos;
	}
	
	private ClassroomSessionDivisionSubjectInfos createClassroomSessionDivisionSubject(Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects,Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes,ClassroomSessionDivision classroomSessionDivision,Subject subject,Object[][] evaluationTypes){
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(classroomSessionDivision,subject,BigDecimal.ONE,teacherDao.readOneRandomly());
		classroomSessionDivisionSubjects.add(classroomSessionDivisionSubject);
		ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos = new ClassroomSessionDivisionSubjectInfos(classroomSessionDivisionSubject);
		for(Object[] evaluationType : evaluationTypes){
			Object[] infos = evaluationType;
			classroomSessionDivisionSubjectInfos.getEvaluationTypes().add(createSubjectEvaluationType(subjectEvaluationTypes,classroomSessionDivisionSubject, (EvaluationType)infos[0], new BigDecimal((String)infos[1])));
		}
		return classroomSessionDivisionSubjectInfos;
	}
	
	private ClassroomSessionDivisionSubjectEvaluationType createSubjectEvaluationType(Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes,ClassroomSessionDivisionSubject subject,EvaluationType name,BigDecimal coefficient){
		ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType = new ClassroomSessionDivisionSubjectEvaluationType(subject,name,coefficient,new BigDecimal("100"));
		Interval interval = new Interval(null, RandomStringUtils.randomAlphanumeric(10), "Count interval", new BigDecimal("1"), new BigDecimal("1"));
		interval.getLow().setExcluded(Boolean.FALSE);
		interval.getHigh().setExcluded(Boolean.FALSE);
		subjectEvaluationType.setCountInterval(interval);
		subjectEvaluationTypes.add(subjectEvaluationType);
		return subjectEvaluationType;
	}
	 
	/**/
		
	public static class ReportProducer extends AbstractSchoolReportProducer{
		private static final long serialVersionUID = 246685915578107971L;
    	
		@Override
		public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
				StudentClassroomSessionDivisionReportParameters parameters) {
			parameters.setRankable(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getStudentClassroomSessionDivisionRankable());
			StudentClassroomSessionDivisionReport r = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,parameters);
			r.getAcademicSession().getCompany().setName("<style forecolor=\"red\">I</style>NTERNATIONAL <style forecolor=\"red\">E</style>NGLISH <style forecolor=\"red\">S</style>CHOOL"
					+ " OF <style forecolor=\"red\">A</style>BIDJAN");
			
			r.getSubjectsTableColumnNames().add("No.");
			r.getSubjectsTableColumnNames().add("SUBJECTS");
			r.getSubjectsTableColumnNames().add("Test 1 15%");
			r.getSubjectsTableColumnNames().add("Test 2 15%");
			r.getSubjectsTableColumnNames().add("Exam 70%");
			r.getSubjectsTableColumnNames().add("TOTAL 100%");
			r.getSubjectsTableColumnNames().add("GRADE");
			r.getSubjectsTableColumnNames().add("RANK");
			r.getSubjectsTableColumnNames().add("OUT OF");
			r.getSubjectsTableColumnNames().add("MAX");
			r.getSubjectsTableColumnNames().add("CLASS AVERAGE");
			r.getSubjectsTableColumnNames().add("REMARKS");
			r.getSubjectsTableColumnNames().add("TEACHER");
			
			r.setInformationLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.informations"));
			if(studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()==2){
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualaverage", "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualgrade", "To Compute");
				labelValue("school.report.studentclassroomsessiondivision.block.informations.annualrank", "To Compute");
				//labelValue("school.report.studentclassroomsessiondivision.block.informations.promotion", 
				//		studentClassroomSessionDivision.get "To Compute");
			}else{
				labelValue("school.report.studentclassroomsessiondivision.block.informations.nextacademicsession", 
						format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNextStartingDate()));
			}
			
			r.setBehaviorLabelValueCollection1(new LabelValueCollectionReport());
			r.getBehaviorLabelValueCollection1().setName("school.report.studentclassroomsessiondivision.block.behaviour");
			for(int i=0;i<=5;i++)
				r.getBehaviorLabelValueCollection1().getCollection().add(r.getBehaviorLabelValueCollection().getCollection().get(i));
			
			r.setBehaviorLabelValueCollection2(new LabelValueCollectionReport());
			r.getBehaviorLabelValueCollection2().setName("school.report.studentclassroomsessiondivision.block.behaviour");
			for(int i=6;i<=11;i++)
				r.getBehaviorLabelValueCollection2().getCollection().add(r.getBehaviorLabelValueCollection().getCollection().get(i));
			
			return r;
		}
		
    }
	
	/**/
	
	@Getter @Setter
	public class ClassroomsessionBusinessProducer extends AbstractBean implements Runnable {

		private static final long serialVersionUID = 925442738199260331L;
		private ClassroomSessionInfos classroomSessionInfos;
		private FakedDataProducerListener listener;
		private Collection<StudentSubject> studentSubjects;
		private Collection<Student> students;
		
		public ClassroomsessionBusinessProducer(ClassroomSessionInfos classroomSessionInfos,FakedDataProducerListener listener,Collection<Student> students,Collection<StudentSubject> studentSubjects) {
			super();
			this.classroomSessionInfos = classroomSessionInfos;
			this.listener = listener;
			this.students = students;
			this.studentSubjects = studentSubjects;
		}
		
		@Override
		public void run() {
			
			//Collection<StudentSubject> studentSubjects = new ArrayList<>();
			//createStudentSubjects(studentSubjects, classroomSessionInfos,students);
			//flush(StudentSubject.class,studentSubjectBusiness,studentSubjects);	
			createStudentClassroomSessions(classroomSessionInfos, students);	
			//createStudentSubjects(studentSubjects, classroomSessionInfos,students);
		}
	}

}
