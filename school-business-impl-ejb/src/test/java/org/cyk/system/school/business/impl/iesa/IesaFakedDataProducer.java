package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.RandomStringUtils;
import org.cyk.system.company.business.api.structure.CompanyBusiness;
import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.CompanyBusinessLayerAdapter;
import org.cyk.system.company.model.structure.Company;
import org.cyk.system.root.business.api.BusinessService.BusinessServiceCallArguments;
import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness;
import org.cyk.system.root.business.api.mathematics.MetricCollectionBusiness;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.party.ApplicationBusinessImpl;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValueInputted;
import org.cyk.system.root.model.mathematics.MetricValueType;
import org.cyk.system.root.model.security.Installation;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionDao;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionStudentsMetricCollectionBusiness;
import org.cyk.system.school.business.api.session.LevelGroupBusiness;
import org.cyk.system.school.business.api.session.LevelGroupTypeBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.EvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper.ClassroomSessionInfos;
import org.cyk.system.school.business.impl.integration.AbstractSchoolFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.ClassroomSessionDivisionStudentsMetricCollection;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
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
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.joda.time.DateTime;
import org.joda.time.DateTimeConstants;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractSchoolFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	public static final String MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR = "BSWHG1G6";
	public static final String MERIC_COLLECTION_G7_G12_STUDENT_BEHAVIOUR = "BSWHG7G12";
	
	public static final String LEVEL_NAME_CODE_PK = "PK";
	public static final String LEVEL_NAME_CODE_K1 = "K1";
	public static final String LEVEL_NAME_CODE_K2 = "K2";
	public static final String LEVEL_NAME_CODE_K3 = "K3";
	public static final String LEVEL_NAME_CODE_G1 = "G1";
	public static final String LEVEL_NAME_CODE_G2 = "G2";
	public static final String LEVEL_NAME_CODE_G3 = "G3";
	public static final String LEVEL_NAME_CODE_G4 = "G4";
	public static final String LEVEL_NAME_CODE_G5 = "G5";
	public static final String LEVEL_NAME_CODE_G6 = "G6";
	public static final String LEVEL_NAME_CODE_G7 = "G7";
	public static final String LEVEL_NAME_CODE_G8 = "G8";
	public static final String LEVEL_NAME_CODE_G9 = "G9";
	public static final String LEVEL_NAME_CODE_G10 = "G10";
	public static final String LEVEL_NAME_CODE_G11 = "G11";
	public static final String LEVEL_NAME_CODE_G12 = "G12";
	
	public static final String REPORT_CYK_GLOBAL_RANKABLE = "CYK_GLOBAL_RANKABLE";
	
	public static final String MERIC_COLLECTION_PK_STUDENT_EXPRESSIVE_LANGUAGE = "MCPKSEL";
	public static final String MERIC_COLLECTION_PK_STUDENT_RECEPTIVE_LANGUAGE = "MCPKSRL";
	public static final String MERIC_COLLECTION_PK_STUDENT_READING_READNESS = "MCPKSRR";
	public static final String MERIC_COLLECTION_PK_STUDENT_NUMERACY_DEVELOPMENT = "MCPKSND";
	public static final String MERIC_COLLECTION_PK_STUDENT_ARTS_MUSIC = "MCPKSAM";
	public static final String MERIC_COLLECTION_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT = "MCPKSSED";
	public static final String MERIC_COLLECTION_PK_STUDENT_GROSS_MOTOR_SKILLS = "MCPKSGMS";
	public static final String MERIC_COLLECTION_PK_STUDENT_FINE_MOTOR_SKILLS = "MCPKSFMS";
	
	public static final String MERIC_COLLECTION_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING = "MCK1SELAR";
	public static final String MERIC_COLLECTION_K1_STUDENT_COMMUNICATION_SKILLS = "MCK1SCS";
	public static final String MERIC_COLLECTION_K1_STUDENT_SCIENCE = "MCK1SS";
	public static final String MERIC_COLLECTION_K1_STUDENT_SOCIAL_STUDIES = "MCK1SSS";
	public static final String MERIC_COLLECTION_K1_STUDENT_MATHEMATICS = "MCK1SM";
	public static final String MERIC_COLLECTION_K1_STUDENT_WORK_HABITS = "MCK1SWH";
	public static final String MERIC_COLLECTION_K1_STUDENT_SOCIAL_SKILLS = "MCK1SSSK";
	
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_READING_READINESS = "MCK2K3SRR";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_READING = "MCK2K3SR";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_WRITING = "MCK2K3SW";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_LISTENING_SPEAKING_VIEWING = "MCK2K3SLSV";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_ALPHABET_IDENTIFICATION = "MCK2K3SAI";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_MATHEMATICS = "MCK2K3SM";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION = "MCK2K3SSSSME";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_ART_CRAFT = "MCK2K3SAC";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_MUSIC = "MCK2K3SMM";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_PHYSICAL_EDUCATION = "MCK2K3SPE";
	public static final String MERIC_COLLECTION_K2_K3_STUDENT_WORK_BEHAVIOUR_HABITS = "MCK2K3SWBH";
	
	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	@Inject private CompanyBusiness companyBusiness;
	@Inject private CompanyBusinessLayer companyBusinessLayer;
	@Inject private EvaluationBusiness subjectEvaluationBusiness;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeBusiness subjectEvaluationTypeBusiness;
	@Inject private ClassroomSessionDivisionSubjectEvaluationTypeDao subjectEvaluationTypeDao;
	@Inject private StudentClassroomSessionDivisionSubjectDao studentSubjectDao;
	@Inject private StudentClassroomSessionDivisionSubjectBusiness studentSubjectBusiness;
	@Inject private StudentBusiness studentBusiness;
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private ClassroomSessionDivisionSubjectBusiness classroomSessionDivisionSubjectBusiness;
	@Inject private ClassroomSessionDivisionSubjectDao classroomSessionDivisionSubjectDao;
	@Inject private ClassroomSessionDivisionStudentsMetricCollectionBusiness classroomSessionDivisionStudentsMetricCollectionBusiness;
	@Inject private LectureBusiness lectureBusiness;
	@Inject private TeacherDao teacherDao;
	@Inject private StudentDao studentDao;
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
	
	private ClassroomSessionInfos pk,k1,k2,k3,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12;
	private MetricCollection[] pkMetricCollections,k1MetricCollections,k2k3MetricCollections
	,g1g6MetricCollections,g7g12MetricCollections;
	
	private Collection<ClassroomSessionDivisionSubject> grade1Subjects = new ArrayList<>();
	private Collection<EvaluationType> evaluationTypes = new ArrayList<>();
	private Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>();
	
	private CommonNodeInformations commonNodeInformations1,commonNodeInformations2;
	
	@Setter private Integer numbreOfTeachers = 0;
	@Setter private Integer numbreOfStudents = 0;
	@Setter private Integer numbreOfLecturesByClassroomSessionDivisionSubject = 5;
	@Setter private Integer numbreOfStudentsByClassroomSession = 25;
	
	@Setter private Boolean generateCompleteAcademicSession = Boolean.FALSE;
	@Setter private Boolean generateStudentClassroomSessionDivisionReport = Boolean.FALSE;
	@Setter private Integer classroomSessionDivisionIndex = 1;
	
	private ArrayList<Subject> subjectsG1G3 = new ArrayList<>(),subjectsG4G6 = new ArrayList<>()
			,subjectsG7G9 = new ArrayList<>(),subjectsG10G12 = new ArrayList<>(); 
	private LevelGroupType levelGroupType; 
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(GlobalIdentifier.FIELD_CODE, Level.class);
		AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(GlobalIdentifier.FIELD_CODE, LevelTimeDivision.class);
		AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(GlobalIdentifier.FIELD_CODE, ClassroomSession.class);
    	AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(GlobalIdentifier.FIELD_CODE, StudentClassroomSession.class);
    	
		ApplicationBusinessImpl.Listener.COLLECTION.add(new ApplicationBusinessImpl.Listener.Adapter.Default(){
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
		SchoolDataProducerHelper.Listener.COLLECTION.add(new SchoolDataProducerHelper.Listener.Adapter.Default(){
			private static final long serialVersionUID = -5301917191935456060L;

			@Override
    		public void classroomSessionDivisionCreated(ClassroomSessionDivision classroomSessionDivision) {
    			super.classroomSessionDivisionCreated(classroomSessionDivision);
    			classroomSessionDivision.getExistencePeriod().setFromDate(new DateTime(2016, 4, 4, 0, 0).toDate());
    			classroomSessionDivision.getExistencePeriod().setToDate(new DateTime(2016, 6, 13, 0, 0).toDate());
    			classroomSessionDivision.setNumberOfMillisecond(48l * DateTimeConstants.MILLIS_PER_DAY);
    			classroomSessionDivision.setStudentSubjectAttendanceAggregated(Boolean.FALSE);
    		}
			
			@Override
			public void classroomSessionDivisionSubjectEvaluationTypeCreated(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
				super.classroomSessionDivisionSubjectEvaluationTypeCreated(classroomSessionDivisionSubjectEvaluationType);
				classroomSessionDivisionSubjectEvaluationType.setCountInterval(inject(IntervalBusiness.class).instanciateOne(null
						, RandomStringUtils.randomAlphanumeric(6), "1", "1"));
			}
    	});
		
		org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionBusinessImpl.Listener.COLLECTION.add(
				new org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionBusinessImpl.Listener.Adapter.Default(){
					private static final long serialVersionUID = 1L;

					@Override
					public void processOnEvaluationAverageUpdated(Collection<ClassroomSessionDivision> classroomSessionDivisions,BusinessServiceCallArguments<StudentClassroomSessionDivision> callArguments) {
						super.processOnEvaluationAverageUpdated(classroomSessionDivisions,callArguments);
						if(classroomSessionDivisions.iterator().next().getClassroomSession().getAcademicSession().getNodeInformations().getCurrentClassroomSessionDivisionIndex().intValue()==2){
							Collection<ClassroomSession> classroomSessions = new HashSet<>();
							for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisions)
								classroomSessions.add(classroomSessionDivision.getClassroomSession());
							
							inject(StudentClassroomSessionBusiness.class).updateAverage(classroomSessions, new BusinessServiceCallArguments<StudentClassroomSession>());
						
							inject(StudentClassroomSessionBusiness.class).updateRank(classroomSessions, inject(SchoolBusinessLayer.class).getStudentEvaluationResultsRankOptions(), new BusinessServiceCallArguments<StudentClassroomSession>());
						}else{
							
						}
					}
				});
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void structure(){
		levelGroupType = create(inject(LevelGroupTypeBusiness.class).instanciateOne("LevelGroupTypeDummy"));
		LevelGroup levelGroupPrimary = (LevelGroup) create(inject(LevelGroupBusiness.class).instanciateOne(SchoolConstant.LEVEL_GROUP_PRIMARY)
				.setType(levelGroupType));
		/*LevelGroup levelGroupSecondary = (LevelGroup) create(inject(LevelGroupBusiness.class).instanciateOne(SchoolConstant.LEVEL_GROUP_SECONDARY)
				.setType(levelGroupType));*/
		
		// Subjects
		schoolDataProducerHelper.createOneSubject("Mathematics",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Grammar",new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("Reading & Comprehension",new ArrayList[]{subjectsG1G3});
    	schoolDataProducerHelper.createOneSubject("Hand writing",new ArrayList[]{subjectsG1G3});
    	schoolDataProducerHelper.createOneSubject("Spelling",new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("Phonics",new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("Creative writing",new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("Moral education",new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("Social Studies",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Science",new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("French",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Art & Craft",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Music",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("ICT",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Physical education",new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	
    	schoolDataProducerHelper.createOneSubject("Litterature",new ArrayList[]{subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("Comprehension",new ArrayList[]{subjectsG4G6});
    	schoolDataProducerHelper.createOneSubject("History",new ArrayList[]{subjectsG4G6,subjectsG7G9,subjectsG10G12});
    	
    	schoolDataProducerHelper.createOneSubject("English Language",new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Literature in english",new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Geography",new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Physics",new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Chemistry",new ArrayList[]{subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Biology",new ArrayList[]{subjectsG7G9});
    	schoolDataProducerHelper.createOneSubject("Spanish",new ArrayList[]{subjectsG7G9});
    	
    	schoolDataProducerHelper.createOneSubject("Sociology",new ArrayList[]{subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Religious studies/Divinity",new ArrayList[]{subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Core mathematics",new ArrayList[]{subjectsG10G12});
    	schoolDataProducerHelper.createOneSubject("Advanced mathematics",new ArrayList[]{subjectsG10G12});
		
		//Evaluation Type
		evaluationTypes.add(evaluationTypeTest1 = create(inject(EvaluationTypeBusiness.class).instanciateOne("Test 1")));
		evaluationTypes.add(evaluationTypeTest2 = create(inject(EvaluationTypeBusiness.class).instanciateOne("Test 2")));
		evaluationTypes.add(evaluationTypeExam = create(inject(EvaluationTypeBusiness.class).instanciateOne("Exam")));
				
		createMetricCollections();
		
    	File reportHeaderFile = createFile("report/iesa/document_header.png","document_header.png");
    	
    	File reportFilePk = createFile("report/iesa/pkg.jrxml", "studentclassroomsessiondivision_pkg.jrxml");
		ReportTemplate reportTemplatePk = new ReportTemplate("SCSDRTPK","PK Marks card",Boolean.TRUE,reportFilePk,reportHeaderFile
				,createFile("report/iesa/studentclassroomsessiondivisionreport_background.jpg","studentclassroomsessiondivisionreport_background.jpg"),null);
		create(reportTemplatePk);
    	
		ReportTemplate reportTemplate = new ReportTemplate("SCSDRT","G Marks card",Boolean.TRUE,createFile("report/iesa/g1g12.jrxml", "reportcard.jrxml"),null,null,null);
		create(reportTemplate);
		
		CommonNodeInformations commonNodeInformationsPk = schoolDataProducerHelper.instanciateOneCommonNodeInformations(null,null, reportTemplatePk, TimeDivisionType.DAY, TimeDivisionType.TRIMESTER,"50", "2");
		
		CommonNodeInformations commonNodeInformationsG1G3 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(create(inject(IntervalCollectionBusiness.class)
				.instanciateOne("G1G6Grade", "Grade", new String[][]{
						{"A+", "Excellent", "90", "100"},{"A", "Very good", "80", "89.99"},{"B+", "Good", "70", "79.99"},{"B", "Fair", "60", "69.99"}
						,{"C+", "Satisfactory", "55", "59.99"},{"C", "Barely satisfactory", "50", "54.99"},{"E", "Fail", "0", "49.99"}})),create(inject(IntervalCollectionBusiness.class)
								.instanciateOne("ICP1", "Promotion Scale", new String[][]{
										{"P", "Promoted", "50", "100"},{"PT", "Promoted on trial", "45", "49.99"},{"NP", "Not promoted", "0", "44.99"}})), reportTemplate
						, TimeDivisionType.DAY, TimeDivisionType.TRIMESTER, "50","2");	
		//CommonNodeInformations commonNodeInformationsG4G6 = commonNodeInformationsG1G3;
		
		/*CommonNodeInformations commonNodeInformationsG7G9 =*/ schoolDataProducerHelper.instanciateOneCommonNodeInformations(create(inject(IntervalCollectionBusiness.class)
				.instanciateOne("G7G12Grade", "Grade", new String[][]{
						{"A*", "Outstanding", "90", "100"},{"A", "Excellent", "80", "89.99"},{"B", "Very Good", "70", "79.99"},{"C", "Good", "60", "69.99"}
						,{"D", "Satisfactory", "50", "59.99"},{"E", "Fail", "0", "49.99"}})),create(inject(IntervalCollectionBusiness.class)
								.instanciateOne("ICP2", "Promotion Scale", new String[][]{
										{"P", "Promoted", "50", "100"},{"PT", "Promoted on trial", "45", "49.99"},{"NP", "Not promoted", "0", "44.99"}})), reportTemplate
						, TimeDivisionType.DAY, TimeDivisionType.TRIMESTER, "50","2");	
		//CommonNodeInformations commonNodeInformationsG10G12 = commonNodeInformationsG7G9;
		
		School school = new School(ownedCompanyBusiness.findDefaultOwnedCompany(),commonNodeInformationsG1G3);
    	create(school);
    	
    	AcademicSession academicSession = new AcademicSession(school,commonNodeInformationsG1G3,new Date());
    	academicSession.getGlobalIdentifierCreateIfNull().getExistencePeriod().setFromDate(new Date());
    	academicSession.getExistencePeriod().setToDate(new Date(academicSession.getExistencePeriod().getFromDate().getTime()+DateTimeConstants.MILLIS_PER_DAY*355));
    	academicSession = create(academicSession);
		
    	inject(TeacherBusiness.class).create(inject(TeacherBusiness.class).instanciateManyRandomly(numbreOfTeachers));
		flush("Teachers");
		inject(StudentBusiness.class).create(inject(StudentBusiness.class).instanciateManyRandomly(numbreOfStudents));
		flush("Students");
		
    	school.getOwnedCompany().getCompany().setManager(personDao.readOneRandomly());
    	companyBusiness.update(school.getOwnedCompany().getCompany());
    	    	
    	Collection<ClassroomSession> classroomSessions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>();
    	Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>(); 
    	Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = new ArrayList<>(); 
    	
    	Integer gradeIndex = 0;
    	
    	pk = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("PK","Pre-Kindergarten",levelGroupPrimary,commonNodeInformationsPk,gradeIndex++) 
    			,null, null,classroomSessionDivisionStudentsMetricCollections,pkMetricCollections,null,Boolean.FALSE,Boolean.FALSE).iterator().next();
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("K1","Kindergarten 1",levelGroupPrimary,commonNodeInformationsPk,gradeIndex++) 
    			, null,null,classroomSessionDivisionStudentsMetricCollections,pkMetricCollections,null,Boolean.FALSE,Boolean.FALSE);
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("K2","Kindergarten 2",levelGroupPrimary,commonNodeInformationsPk,gradeIndex++) 
    			, null,null,classroomSessionDivisionStudentsMetricCollections,pkMetricCollections,null,Boolean.FALSE,Boolean.FALSE);
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("K3","Kindergarten 3",levelGroupPrimary,commonNodeInformationsPk,gradeIndex++) 
    			, null,null,classroomSessionDivisionStudentsMetricCollections,pkMetricCollections,null,Boolean.FALSE,Boolean.FALSE);
    	
    	g1 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G1","Grade 1",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG1G3,classroomSessionDivisionStudentsMetricCollections
    			,g1g6MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();    	
    	/*g2 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G2","Grade 2",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG1G3,classroomSessionDivisionStudentsMetricCollections
    			,g1g6MetricCollections,null,Boolean.TRUE,Boolean.TRUE).iterator().next();
    	g3 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G3","Grade 3",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}},subjectsG1G3,classroomSessionDivisionStudentsMetricCollections
    			,g1g6MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();
    	
    	g4 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G4","Grade 4",levelGroupPrimary,commonNodeInformationsG4G6,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG4G6,classroomSessionDivisionStudentsMetricCollections
    			,g1g6MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();    	
    	g5 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G5","Grade 5",levelGroupPrimary,commonNodeInformationsG4G6,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG4G6,classroomSessionDivisionStudentsMetricCollections
    			,g1g6MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();
    	g6 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G6","Grade 6",levelGroupPrimary,commonNodeInformationsG4G6,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}},subjectsG4G6,classroomSessionDivisionStudentsMetricCollections
    			,g1g6MetricCollections,null,Boolean.TRUE,Boolean.TRUE).iterator().next();
    	
    	g7 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G7","Grade 7",levelGroupSecondary,commonNodeInformationsG7G9,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}}, subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();    	
    	g8 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G8","Grade 8",levelGroupSecondary,commonNodeInformationsG7G9,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}}, subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,null,Boolean.TRUE,Boolean.TRUE).iterator().next();
    	g9 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G9","Grade 9",levelGroupSecondary,commonNodeInformationsG7G9,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}},subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();
    	
    	g10 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G10","Grade 10",levelGroupSecondary,commonNodeInformationsG10G12,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}}, subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.FALSE).iterator().next();    	
    	g11 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G11","Grade 12",levelGroupSecondary,commonNodeInformationsG10G12,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}}, subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,null,Boolean.TRUE,Boolean.FALSE).iterator().next();
    	g12 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G12","Grade 12",levelGroupSecondary,commonNodeInformationsG10G12,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}},subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.FALSE).iterator().next();
    	*/
    	flush(ClassroomSession.class, classroomSessionBusiness, classroomSessions);
    	flush(ClassroomSessionDivision.class, classroomSessionDivisionBusiness, classroomSessionDivisions);
    	flush(ClassroomSessionDivisionSubject.class, classroomSessionDivisionSubjectBusiness, classroomSessionDivisionSubjects);
    	flush(ClassroomSessionDivisionSubjectEvaluationType.class, subjectEvaluationTypeBusiness, subjectEvaluationTypes);
    	flush(ClassroomSessionDivisionStudentsMetricCollection.class, classroomSessionDivisionStudentsMetricCollectionBusiness, classroomSessionDivisionStudentsMetricCollections);
	}
	
	@Override
	public void produce(Listener listener) {
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
	protected void doBusiness(Listener listener){
		//ExecutorService executor = Executors.newFixedThreadPool(5);
		//Collection<StudentSubject> studentSubjects = new ArrayList<>();
		for(ClassroomSessionInfos classroomSessionInfos : new ClassroomSessionInfos[]{g1,g2,g3,g4,g5,g6,g7,g8,g9}){
			Collection<Student> students = studentDao.readManyRandomly(numbreOfStudentsByClassroomSession);
			createStudentClassroomSessions(classroomSessionInfos, students);	
			//executor.execute(new ClassroomsessionBusinessProducer(classroomSessionInfos, listener, students,studentSubjects));
		}
		//executor.shutdown();
        //while (!executor.isTerminated()) {}
		
		//flush(StudentSubject.class,studentSubjectBusiness,studentSubjects);
		
		Collection<Evaluation> subjectEvaluations = new ArrayList<>();
		for(ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType : subjectEvaluationTypeDao.readAll()){
			Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType);
			subjectEvaluation.setCoefficientApplied(Boolean.FALSE);
			for(StudentClassroomSessionDivisionSubject studentSubject :studentSubjectDao.readByClassroomSessionDivisionSubject(subjectEvaluationType.getClassroomSessionDivisionSubject()) ){
				StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation = new StudentClassroomSessionDivisionSubjectEvaluation(subjectEvaluation, studentSubject
						, new BigDecimal(RandomDataProvider.getInstance().randomInt(0, subjectEvaluationType.getMaximumValue().intValue())));
				subjectEvaluation.getStudentSubjectEvaluations().add(studentSubjectEvaluation);
			}
			subjectEvaluations.add(subjectEvaluation);
			flush(Evaluation.class,subjectEvaluationBusiness,subjectEvaluations,10000l);
		}
		flush(Evaluation.class,subjectEvaluationBusiness,subjectEvaluations);
		/*
		Collection<Lecture> lectures = new ArrayList<>();
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjectDao.readAll()){
			for(int i=0;i<numbreOfLecturesByClassroomSessionDivisionSubject;i++){
				Event event = new Event();
				event.getExistencePeriod().setFromDate(new Date());
				event.getExistencePeriod().setToDate(new Date());
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
		*/
	
		/*
		if(Boolean.TRUE.equals(generateStudentClassroomSessionDivisionReport)){
			System.out.println("Updating metric value");
			
			Collection<ClassroomSessionDivision> classroomSessionInfos = Arrays.asList(pk.division(classroomSessionDivisionIndex).getClassroomSessionDivision(),g1.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					,g2.division(classroomSessionDivisionIndex).getClassroomSessionDivision(),g3.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					,g4.division(classroomSessionDivisionIndex).getClassroomSessionDivision(),g5.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					,g6.division(classroomSessionDivisionIndex).getClassroomSessionDivision(),g7.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					,g8.division(classroomSessionDivisionIndex).getClassroomSessionDivision(),g9.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					,g10.division(classroomSessionDivisionIndex).getClassroomSessionDivision(),g11.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					,g12.division(classroomSessionDivisionIndex).getClassroomSessionDivision()
					);
			SchoolBusinessTestHelper.getInstance().randomValues(classroomSessionInfos, Boolean.TRUE, Boolean.TRUE,Boolean.TRUE);
			
			System.out.println("Generating report");
			inject(StudentClassroomSessionDivisionBusiness.class).buildReport(classroomSessionInfos);
		}
		*/
	}

	private void createStudentClassroomSessions(ClassroomSessionInfos classroomSessionInfos,Collection<Student> students){
		System.out.println("Creating data of classroom session "+classroomSessionInfos.getClassroomSession().getIdentifier()+" with "+students.size()+" students");
		for(Student student : students){
			inject(StudentClassroomSessionBusiness.class).create(new StudentClassroomSession(student, classroomSessionInfos.getClassroomSession()));	
		}
	}
	
	/**/
		 
	/**/
		
	public static class ReportProducer extends AbstractSchoolReportProducer{
		private static final long serialVersionUID = 246685915578107971L;
    	
		@Override
		public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
				StudentClassroomSessionDivisionReportParameters parameters) {
			LabelValueCollectionReport labelValueCollectionReport;
			StudentClassroomSessionDivisionReport report = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,parameters);
			
			if(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getLevelTimeDivision().getIndex()>3){
			
				report.addSubjectsTableColumnNames("No.","SUBJECTS","Test 1 15%","Test 2 15%","Exam 70%","TOTAL 100%","GRADE","RANK","OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
				
				report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
						{"Formname(s)", report.getStudent().getPerson().getNames()}
						,{"Surname", report.getStudent().getPerson().getSurname()}
						/*,{"Date of birth", report.getStudent().getPerson().getBirthDate()}
						,{"Place of birth", report.getStudent().getPerson().getBirthLocation()}
						,{"Admission No", report.getStudent().getRegistrationCode()}*/
						,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
						,{"Gender", report.getStudent().getPerson().getSex()}
						});
				
				report.addLabelValueCollection("SCHOOL ATTENDANCE",new String[][]{
						{"Number of times school opened",report.getClassroomSessionDivision().getOpenedTime()}
						,{"Number of times present",report.getAttendedTime()}
						,{"Number of times absent",report.getMissedTime()}
						});
				
				
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName("OVERALL RESULT");
				labelValueCollectionReport.add("AVERAGE",report.getAverage());
				labelValueCollectionReport.add("GRADE",report.getAverageScale());
				if(Boolean.TRUE.equals(studentClassroomSessionDivision.getClassroomSessionDivision().getStudentRankable()))
					labelValueCollectionReport.add("RANK",report.getRank());
				report.addLabelValueCollection(labelValueCollectionReport);
				
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(),MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR);
				labelValueCollectionReport = new LabelValueCollectionReport();
				labelValueCollectionReport.setName(report.getCurrentLabelValueCollection().getName());
				labelValueCollectionReport.setCollection(report.getCurrentLabelValueCollection().getCollection().subList(6, 12));
				report.getCurrentLabelValueCollection().setCollection(report.getCurrentLabelValueCollection().getCollection().subList(0, 6));
				
				report.addLabelValueCollection(labelValueCollectionReport);
				
				addIntervalCollectionLabelValueCollection(report,inject(ClassroomSessionBusiness.class).findCommonNodeInformations(
					((StudentClassroomSessionDivision)report.getSource()).getClassroomSessionDivision().getClassroomSession()).getStudentClassroomSessionDivisionAverageScale()
					,Boolean.FALSE,Boolean.TRUE,new Integer[][]{{1,2}});
				
				addIntervalCollectionLabelValueCollection(report,inject(MetricCollectionDao.class).read(MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR).getValueIntervalCollection()
						,Boolean.TRUE,Boolean.FALSE,null);
				
				if(studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()==2){
					/*labelValue("school.report.studentclassroomsessiondivision.block.informations.annualaverage", "To Compute");
					labelValue("school.report.studentclassroomsessiondivision.block.informations.annualgrade", "To Compute");
					labelValue("school.report.studentclassroomsessiondivision.block.informations.annualrank", "To Compute");
					//labelValue("school.report.studentclassroomsessiondivision.block.informations.promotion", 
					//		studentClassroomSessionDivision.get "To Compute");
					labelValue("school.report.studentclassroomsessiondivision.block.informations.nextacademicsession", 
							format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getNextStartingDate()));
					*/
					StudentResults classroomSessionResults = inject(StudentClassroomSessionDao.class)
							.readByStudentByClassroomSession(studentClassroomSessionDivision.getStudent(), studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()).getResults();
					
					report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{
							{"ANNUAL AVERAGE",format(classroomSessionResults.getEvaluationSort().getAverage().getValue())}
							,{"ANNUAL GRADE"
								,classroomSessionResults.getEvaluationSort().getAverageAppreciatedInterval()==null?NULL_VALUE:inject(IntervalBusiness.class).findRelativeCode(classroomSessionResults.getEvaluationSort().getAverageAppreciatedInterval())}
							,{"ANNUAL RANK",inject(MathematicsBusiness.class).format(classroomSessionResults.getEvaluationSort().getRank())}
							,{"NEXT ACADEMIC SESSION","TO COMPUTE"}
							});
				}else{
					ClassroomSessionDivision nextClassroomSessionDivision = inject(ClassroomSessionDivisionDao.class)
							.readByClassroomSessionByIndex(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession()
									,new Byte((byte) (studentClassroomSessionDivision.getClassroomSessionDivision().getIndex()+1)));
				
					report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{
						{"CONFERENCE REQUESTED",studentClassroomSessionDivision.getResults().getConferenceRequested()==null?"NO"
								:studentClassroomSessionDivision.getResults().getConferenceRequested()?"YES":"NO"}
						,{"NEXT OPENING",format(nextClassroomSessionDivision.getExistencePeriod().getFromDate())}
						,{"NEXT TERM EXAMINATION",format(nextClassroomSessionDivision.getExistencePeriod().getToDate())}
						});
				}
				/*
				report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{
						{"ANNUAL AVERAGE","90"}
						,{"ANNUAL GRADE","B+"}
						,{"ANNUAL RANK","25"}
						,{"PROMOTION INFORMATION","PROMOTED"}
						,{"NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015"}
						});
				*/
			}else{
				/*addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK);
				*/
			}
			return report;
		}
		
    }
	
	/**/
	
	@Getter @Setter
	public class ClassroomsessionBusinessProducer extends AbstractBean implements Runnable {

		private static final long serialVersionUID = 925442738199260331L;
		private ClassroomSessionInfos classroomSessionInfos;
		private Listener listener;
		private Collection<StudentClassroomSessionDivisionSubject> studentSubjects;
		private Collection<Student> students;
		
		public ClassroomsessionBusinessProducer(ClassroomSessionInfos classroomSessionInfos,Listener listener,Collection<Student> students,Collection<StudentClassroomSessionDivisionSubject> studentSubjects) {
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
	
	private void createMetricCollections(){
		String[][] valueIntervals = new String[][]{ {"1", "Learning to do", "1", "1"},{"2", "Does sometimes", "2", "2"} ,{"3", "Does regularly", "3", "3"} };
		pkMetricCollections = new MetricCollection[]{ create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Expressive language",MetricValueType.NUMBER
    			, new String[]{"Participates actively during circle time","Participates in singing rhymes","Can say her name and name of classmates"
    			,"Can respond appropriately to “how are you?”","Can say his/her age","Can say the name of her school","Names objects in the classroom and school environment"
    			,"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","Talks in two or three word phrases and longer sentences"
    			,"Can use “and” to connect words/phrases","Talks with words in correct order","Can be engaged in conversations"}
    	,"Skills Performance levels", valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Receptive language",MetricValueType.NUMBER
    			, new String[]{"Responds to her name when called",
    			"Retrieves named objects",
    			"Follows simple instructions (across the classroom) – stand, sit, bring your cup",
    			"Understands facial expressions and tone of voice",
    			"Understands 2-3 step instructions",
    			"Understands positional words – In and out - Up and down - On and under - Forward and backward",
    			"Understands the concept “Give and Take”",
    			"Talks about feelings"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Reading readness",MetricValueType.NUMBER
    			, new String[]{"Shows interest in books/stories",
    			"Names familiar objects in pictures/books – vegetables, fruits, animals",
    			"Tells what action is going on in pictures",
    			"Handling books – carrying a book, turning the pages of a book, placing a book back in the shelf",
    			"Listening for different sounds in the environment",
    			"Identifying objects that begin with a particular sound",
    			"Identifying pictures that begin with a particular sound",
    			"Recognizes the written letters of the alphabet"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Numeracy development",MetricValueType.NUMBER
    			, new String[]{"Sorts objects by shape",
    			"Sorts objects by size",
    			"Participates in reciting different counting rhymes, songs, stories and games",
    			"Verbally count forward to 10",
    			"Can count 1-10 objects",
    			"Identifies the written numerals 1-10",
    			"Reproducing Patterns",
    			"Identifies the 3 basic geometric shapes ( circle, triangle and square)",
    			"Identifies more shapes ( Star, diamond, heart, cross ,crescent)"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Arts and music",MetricValueType.NUMBER
    			, new String[]{"Moves expressively to sounds and music – nodding, clapping, movement of body",
    			"Participates in musical activities",
    			"Hums or sing words of songs",
    			"Participates in role play",
    			"Shows satisfaction with completed work"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Social and emotional development",MetricValueType.NUMBER
    			, new String[]{"Initiates interaction with adults",
    			"Initiates interaction with classmates",
    			"Participates in group activities",
    			"Takes turns during group activities",
    			"Greets people – hello and goodbye",
    			"Says “please” and “thank you”",
    			"Asks for help in doing things when needed",
    			"Shows sympathy, offers to help or helps others",
    			"Can express dissatisfaction and other emotions – body language or words",
    			"Responds to correction – stops the misbehaviour"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Gross motor skills",MetricValueType.NUMBER
    			, new String[]{"Can run well without falling",
    			"Can kick a ball",
    			"Climbs up ladder and slides down slide without help",
    			"Walks up and down stairs unassisted",
    			"Can stand on one foot for a few seconds without support",
    			"Throws a ball into a basket from a short distance"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Fine motor skills",MetricValueType.NUMBER
    			, new String[]{"Scribbles spontaneously",
    			"Can scribble to and from, in circular motions and in lines",
    			"Can place simple pieces in a puzzle board",
    			"Can build a tower of at least 3-5 blocks",
    			"Develops good pencil grip and control"}
    	, valueIntervals))
    	};		
    	
		valueIntervals = new String[][]{ {"1", "Emerging", "1", "1"}
    	,{"2", "Developing", "2", "2"} 
    	,{"3", "Proficient", "3", "3"},{"4", "Exemplary", "4", "4"} };
		k1MetricCollections = new MetricCollection[]{ create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"English/language/Arts/Reading",MetricValueType.NUMBER
    			, new String[]{"Reads independently with understanding","Comprehends a variety of texts","Applies a variety of strategies to comprehend printed text"
    					,"Reads to access and utilize information from written and electronic sources","Demonstrates understanding of letter-sound associations"}
    	,"Skills Performance levels", valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Communication skills",MetricValueType.NUMBER
    			, new String[]{"Contributes ideas to discussions","Communicates ideas effectively","Write for a variety of purposes","Writes well-organized compositions"
    					,"Uses appropriate writing skills","Write legibly","Revises, edits and proofreads work"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Science",MetricValueType.NUMBER
    			, new String[]{"Understands and applies scientific process","Understands and applies knowledge of key concepts"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Social Studies",MetricValueType.NUMBER
    			, new String[]{"Gathers and organizes information","Understands and applies knowledge of key concepts"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Mathematics",MetricValueType.NUMBER
    			, new String[]{"Demonstrates understanding of number sense","Reads and interprets data","Applies problem-solving strategies","Communicates mathematically"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Work habits",MetricValueType.NUMBER
    			, new String[]{"Follows directions","Uses time and materials constructively ","Works independently","Completes class assignments","Completes homework assignments",
    			"Listens attentively"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Social Skills",MetricValueType.NUMBER
    			, new String[]{"Cooperates with others","Shows respect for others","Participates in classroom activities","Follows classroom/school rules"}
    	, valueIntervals))
    	};
		
		valueIntervals = new String[][]{ {"1", "Does not meets and applies expectations/standards; shows no growth even with support", "1", "1"}
    	,{"2", "Does not meets and applies expectations/standards; but shows growth with support", "2", "2"} 
    	,{"3", "Meets and applies expectations/standards with support", "3", "3"},{"4", "Meets and applies expectations/standards with support", "4", "4"} };
    	
		k2k3MetricCollections = new MetricCollection[]{ create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Reading Readiness",MetricValueType.NUMBER
    			, new String[]{"Demonstrates concepts of print","Identifies and produces rhyming words","Segments and blends sounds"}
    	,"Performance Codes", valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Reading",MetricValueType.NUMBER
    			, new String[]{"Answers questions about essential narrative elements","Reads high frequency words","Blends sounds to read words","Reads simple text"
    					,"Developmental Reading assessment"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Writing",MetricValueType.NUMBER
    			, new String[]{"Writes first and last name","Expresses ideas through independent writing"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Listening,Speaking and Viewing",MetricValueType.NUMBER
    			, new String[]{"Uses oral language to communicate effectively","Recites short poems and songs","Follows two-step oral directions"
    					,"Makes predictions and retells","Comprehends information through listening","Demonstrates comprehension of information through speaking"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Alphabet identification",MetricValueType.NUMBER
    			, new String[]{"Identifies Upper-Case","Identifies Lower-Case","Produces Letter Sounds","Prints Letters Correctly"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Mathematics",MetricValueType.NUMBER
    			, new String[]{"Number and Operations","Geometry","Measurement","Algebraic Thinking"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Science, Social Studies and Moral Education",MetricValueType.NUMBER
    			, new String[]{"Science","Social Studies","Moral Education"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Art and Craft",MetricValueType.NUMBER
    			, new String[]{"Performance","Initiative"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Music",MetricValueType.NUMBER
    			, new String[]{"Performance","Initiative"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Physical Education",MetricValueType.NUMBER
    			, new String[]{"Performance","Initiative"}
    	, valueIntervals))
    	,create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Work and Behaviour Habits",MetricValueType.NUMBER
    			, new String[]{"Follows directions","Uses time and materials constructively","Works independently","Completes class assignments"
    					,"Completes homework assignments","Listens attentively","Cooperates with others","Shows respect for others","Participates in classroom activities"
    					,"Follows classroom/school rules"}
    	, valueIntervals))
    	};
		
		g1g6MetricCollections = new MetricCollection[]{ create(inject(MetricCollectionBusiness.class).instanciateOne(MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR,"Behaviour,Study and Work Habits",MetricValueType.NUMBER
    			, new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies","Demonstrates self-control"
    					,"Takes care of school and others materials","Game/Sport","Handwriting","Drawing/Painting","Punctionality/Regularity","Works cooperatively in groups"
    					,"Listens and follows directions"}
    	,"Effort Levels", new String[][]{ {"1", "Has no regard for the observable traits", "1", "1"},{"2", "Shows minimal regard for the observable traits", "2", "2"}
    	,{"3", "Acceptable level of observable traits", "3", "3"},{"4", "Maintains high level of observable traits", "4", "4"}
    	,{"5", "Maintains an excellent degree of observable traits", "5", "5"} }))};
   
		g7g12MetricCollections = new MetricCollection[]{ create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Behaviour,Study and Work Habits",MetricValueType.STRING
    			, new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies","Demonstrates self-control"
    					,"Takes care of school and others materials","Game/Sport","Handwriting","Drawing/Painting","Punctionality/Regularity","Works cooperatively in groups"
    					,"Listens and follows directions"}
    	,"Effort Levels", new String[][]{ {"E", "Excellent", "1", "1"},{"G", "Good", "2", "2"},{"S", "Satisfactory", "3", "3"},{"N", "Needs Improvement", "4", "4"}
    	,{"H", "Has no regard", "5", "5"} }).setMetricValueInputted(MetricValueInputted.VALUE_INTERVAL_CODE))};
	}

}
