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
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.Metric;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricValueType;
import org.cyk.system.root.model.security.Installation;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionStudentsMetricCollectionBusiness;
import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper.ClassroomSessionInfos;
import org.cyk.system.school.business.impl.integration.AbstractSchoolFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
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
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.joda.time.DateTimeConstants;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractSchoolFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	public static final String STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK = "BSWHPK";
	public static final String STUDENT_BEHAVIOUR_MERIC_COLLECTION_G1_G6 = "BSWHG1G6";
	public static final String STUDENT_BEHAVIOUR_MERIC_COLLECTION_G7_G12 = "BSWHG7G12";
	
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
	@Inject private ClassroomSessionDivisionStudentsMetricCollectionBusiness classroomSessionDivisionStudentsMetricCollectionBusiness;
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
	
	private MetricCollection studentWorkMetricCollection1,studentWorkMetricCollection2;
	
	private CommonNodeInformations commonNodeInformations1,commonNodeInformations2;
	
	@Setter private Integer numbreOfTeachers = 10;
	@Setter private Integer numbreOfStudents = 10;
	@Setter private Integer numbreOfLecturesByClassroomSessionDivisionSubject = 5;
	@Setter private Integer numbreOfStudentsByClassroomSession = 25;
	
	@Setter private Boolean generateCompleteAcademicSession = Boolean.FALSE;
	@Setter private Boolean generateStudentClassroomSessionDivisionReport = Boolean.FALSE;
	
	private MetricCollection studentWorkMetricCollectionPk,studentWorkMetricCollectionK1,studentWorkMetricCollectionK2,studentWorkMetricCollectionK3
		,studentWorkMetricCollectionG1G6,studentWorkMetricCollectionG7G12;
	
	private ArrayList<Subject> subjectsG1G3 = new ArrayList<>(),subjectsG4G6 = new ArrayList<>()
			,subjectsG7G9 = new ArrayList<>(),subjectsG10G12 = new ArrayList<>(); 
	private LevelGroupType levelGroupType; 
	
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
	
	@SuppressWarnings("unchecked")
	@Override
	protected void structure(){
		levelGroupType = create(schoolBusinessLayer.getLevelGroupTypeBusiness().instanciateOne("LevelGroupTypeDummy"));
		LevelGroup levelGroupPrimary = (LevelGroup) create(schoolBusinessLayer.getLevelGroupBusiness().instanciateOne(SchoolConstant.LEVEL_GROUP_PRIMARY)
				.setType(levelGroupType));
		LevelGroup levelGroupSecondary = (LevelGroup) create(schoolBusinessLayer.getLevelGroupBusiness().instanciateOne(SchoolConstant.LEVEL_GROUP_SECONDARY)
				.setType(levelGroupType));
		
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
		evaluationTypes.add(evaluationTypeTest1 = create(schoolBusinessLayer.getEvaluationTypeBusiness().instanciateOne("Test 1")));
		evaluationTypes.add(evaluationTypeTest2 = create(schoolBusinessLayer.getEvaluationTypeBusiness().instanciateOne("Test 2")));
		evaluationTypes.add(evaluationTypeExam = create(schoolBusinessLayer.getEvaluationTypeBusiness().instanciateOne("Exam")));
				
		studentWorkMetricCollectionPk = create(rootBusinessLayer.getMetricCollectionBusiness().instanciateOne(STUDENT_BEHAVIOUR_MERIC_COLLECTION_PK,"Behaviour,Study and Work Habits",MetricValueType.NUMBER
    			, new String[]{"Participates actively during circle time","Participates in singing rhymes","Can say her name and name of classmates"
    			,"Can respond appropriately to “how are you?”","Can say his/her age","Can say the name of her school","Names objects in the classroom and school environment"
    			,"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","Talks in two or three word phrases and longer sentences"
    			,"Can use “and” to connect words/phrases","Talks with words in correct order","Can be engaged in conversations"}
    	, new String[][]{ {"BSWHPk_1", "Learning to do", "1", "1"},{"BSWHPk_2", "Does sometimes", "2", "2"} ,{"BSWHPk_3", "Does regularly", "3", "3"} }));
    	
    	studentWorkMetricCollectionG1G6 = create(rootBusinessLayer.getMetricCollectionBusiness().instanciateOne(STUDENT_BEHAVIOUR_MERIC_COLLECTION_G1_G6,"Behaviour,Study and Work Habits",MetricValueType.NUMBER
    			, new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies","Demonstrates self-control"
    					,"Takes care of school and others materials","Game/Sport","Handwriting","Drawing/Painting","Punctionality/Regularity","Works cooperatively in groups"
    					,"Listens and follows directions"}
    	, new String[][]{ {"1", "Has no regard for the observable traits", "1", "1"},{"2", "Shows minimal regard for the observable traits", "2", "2"}
    	,{"3", "Acceptable level of observable traits", "3", "3"},{"4", "Maintains high level of observable traits", "4", "4"}
    	,{"5", "Maintains an excellent degree of observable traits", "5", "5"} }));
   
    	studentWorkMetricCollectionG7G12 = create(rootBusinessLayer.getMetricCollectionBusiness().instanciateOne(STUDENT_BEHAVIOUR_MERIC_COLLECTION_G7_G12,"Behaviour,Study and Work Habits",MetricValueType.STRING
    			, new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies","Demonstrates self-control"
    					,"Takes care of school and others materials","Game/Sport","Handwriting","Drawing/Painting","Punctionality/Regularity","Works cooperatively in groups"
    					,"Listens and follows directions"}
    	, new String[][]{ {"E", "Excellent", "1", "1"},{"G", "Good", "2", "2"}
    	,{"S", "Satisfactory", "3", "3"},{"N", "Needs Improvement", "4", "4"}
    	,{"H", "Has no regard", "5", "5"} }));
		
    	File reportHeaderFile = createFile("report/iesa/document_header.png","document_header.png");
    	
    	File reportFilePk = createFile("report/iesa/pkg.jrxml", "studentclassroomsessiondivision_pkg.jrxml");
		ReportTemplate reportTemplatePk = new ReportTemplate("SCSDRTPK",reportFilePk,reportHeaderFile
				,createFile("report/iesa/studentclassroomsessiondivisionreport_background.jpg","studentclassroomsessiondivisionreport_background.jpg"));
		create(reportTemplatePk);
    	
		ReportTemplate reportTemplate = new ReportTemplate("SCSDRT",createFile("report/iesa/iesa.jrxml", "reportcard.jrxml"),null,null);
		create(reportTemplate);
		
		CommonNodeInformations commonNodeInformationsPk = schoolDataProducerHelper.instanciateOneCommonNodeInformations(null, reportTemplatePk, TimeDivisionType.DAY, TimeDivisionType.TRIMESTER, "1");
		
		CommonNodeInformations commonNodeInformationsG1G3 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(create(rootBusinessLayer.getIntervalCollectionBusiness()
				.instanciateOne("ICEV1", "ICEV1", new String[][]{
						{"A+", "Excellent", "90", "100"},{"A", "Very good", "80", "89.99"},{"B+", "Good", "70", "79.99"},{"B", "Fair", "60", "69.99"}
						,{"C+", "Satisfactory", "55", "59.99"},{"C", "Barely satisfactory", "50", "54.99"},{"E", "Fail", "0", "49.99"}})), reportTemplate
						, TimeDivisionType.DAY, TimeDivisionType.TRIMESTER, "1");	
		CommonNodeInformations commonNodeInformationsG4G6 = commonNodeInformationsG1G3;
		
		CommonNodeInformations commonNodeInformationsG7G9 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(create(rootBusinessLayer.getIntervalCollectionBusiness()
				.instanciateOne("ICEV2", "ICEV2", new String[][]{
						{"A*", "Outstanding", "90", "100"},{"A", "Excellent", "80", "89.99"},{"B", "Very Good", "70", "79.99"},{"C", "Good", "60", "69.99"}
						,{"D", "Satisfactory", "50", "59.99"},{"E", "Fail", "0", "49.99"}})), reportTemplate
						, TimeDivisionType.DAY, TimeDivisionType.TRIMESTER, "1");	
		CommonNodeInformations commonNodeInformationsG10G12 = commonNodeInformationsG7G9;
		
		School school = new School(ownedCompanyBusiness.findDefaultOwnedCompany(),commonNodeInformationsG1G3);
    	create(school);
    	
    	AcademicSession academicSession = new AcademicSession(school,commonNodeInformationsG1G3,new Date());
    	academicSession.getPeriod().setFromDate(new Date());
    	academicSession.getPeriod().setToDate(new Date(academicSession.getPeriod().getFromDate().getTime()+DateTimeConstants.MILLIS_PER_DAY*355));
    	academicSession = create(academicSession);
		
		rootRandomDataProvider.createActor(Teacher.class, numbreOfTeachers);
		flush("Teachers");
		rootRandomDataProvider.createActor(Student.class, numbreOfStudents);
		flush("Students");
		
    	school.getOwnedCompany().getCompany().setManager(personDao.readOneRandomly());
    	companyBusiness.update(school.getOwnedCompany().getCompany());
    	    	
    	Collection<ClassroomSession> classroomSessions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>();
    	Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>(); 
    	Collection<ClassroomSessionDivisionStudentsMetricCollection> classroomSessionDivisionStudentsMetricCollections = new ArrayList<>(); 
    	
    	Integer gradeIndex = 0;
    	
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("PK","Pre-Kindergarten",levelGroupPrimary,commonNodeInformationsPk,gradeIndex++) 
    			,null, null,classroomSessionDivisionStudentsMetricCollections,new MetricCollection[]{studentWorkMetricCollectionPk},null,Boolean.FALSE,Boolean.FALSE);
    	/*schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, createLevelTimeDivision(IesaConstant.LEVEL_NAME_CODE_K1,"Kindergarten 1",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) , null,null,Boolean.TRUE,Boolean.TRUE);
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, createLevelTimeDivision(IesaConstant.LEVEL_NAME_CODE_K2,"Kindergarten 2",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) , null,null,Boolean.TRUE,Boolean.TRUE);
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, createLevelTimeDivision(IesaConstant.LEVEL_NAME_CODE_K3,"Kindergarten 3",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) , null,null,Boolean.TRUE,Boolean.TRUE);
    	*/
    	grade1 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G1","Grade 1",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG1G3,classroomSessionDivisionStudentsMetricCollections
    			,new MetricCollection[]{studentWorkMetricCollectionG1G6},new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();    	
    	grade2 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G2","Grade 2",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG1G3,classroomSessionDivisionStudentsMetricCollections
    			,new MetricCollection[]{studentWorkMetricCollectionG1G6},null,Boolean.TRUE,Boolean.TRUE).iterator().next();
    	grade3 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G3","Grade 3",levelGroupPrimary,commonNodeInformationsG1G3,gradeIndex++) 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}},subjectsG1G3,classroomSessionDivisionStudentsMetricCollections
    			,new MetricCollection[]{studentWorkMetricCollectionG1G6},new String[]{"A","B"},Boolean.TRUE,Boolean.TRUE).iterator().next();
    	
    	flush(ClassroomSession.class, classroomSessionBusiness, classroomSessions);
    	flush(ClassroomSessionDivision.class, classroomSessionDivisionBusiness, classroomSessionDivisions);
    	flush(ClassroomSessionDivisionSubject.class, classroomSessionDivisionSubjectBusiness, classroomSessionDivisionSubjects);
    	flush(ClassroomSessionDivisionSubjectEvaluationType.class, subjectEvaluationTypeBusiness, subjectEvaluationTypes);
    	flush(ClassroomSessionDivisionStudentsMetricCollection.class, classroomSessionDivisionStudentsMetricCollectionBusiness, classroomSessionDivisionStudentsMetricCollections);
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
			Evaluation subjectEvaluation = new Evaluation(subjectEvaluationType);
			subjectEvaluation.setCoefficientApplied(Boolean.FALSE);
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
		 
	/**/
		
	public static class ReportProducer extends AbstractSchoolReportProducer{
		private static final long serialVersionUID = 246685915578107971L;
    	
		@Override
		public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
				StudentClassroomSessionDivisionReportParameters parameters) {
			LabelValueCollectionReport labelValueCollectionReport;
			StudentClassroomSessionDivisionReport report = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision,parameters);
			report.getAcademicSession().getCompany().setName("<style forecolor=\"red\">I</style>NTERNATIONAL <style forecolor=\"red\">E</style>NGLISH <style forecolor=\"red\">S</style>CHOOL"
					+ " OF <style forecolor=\"red\">A</style>BIDJAN");
			
			report.addSubjectsTableColumnNames("No.","SUBJECTS","Test 1 15%","Test 2 15%","Exam 70%","TOTAL 100%","GRADE","RANK","OUT OF","MAX","CLASS AVERAGE","REMARKS","TEACHER");
			

			report.addLabelValueCollection("PUPIL'S DETAILS",new String[][]{
					{"Formname(s)", report.getStudent().getPerson().getNames()}
					,{"Surname", report.getStudent().getPerson().getSurname()}
					,{"Date of birth", report.getStudent().getPerson().getBirthDate()}
					,{"Place of birth", report.getStudent().getPerson().getBirthLocation()}
					,{"Admission No", report.getStudent().getRegistrationCode()}
					,{"Class", report.getClassroomSessionDivision().getClassroomSession().getName()}
					,{"Gender", report.getStudent().getPerson().getSex()}
					});
			
			report.addLabelValueCollection("SCHOOL ATTENDANCE",new String[][]{
					{"Number of times school opened",report.getClassroomSessionDivision().getOpenedTime()}
					,{"Number of times present",report.getAttendedTime()}
					,{"Number of times absent",report.getMissedTime()}
					});
			
			report.addLabelValueCollection("OVERALL RESULT",new String[][]{
					{"AVERAGE",report.getAverage()}
					,{"GRADE",report.getAverageScale()}
					,{"RANK",report.getRank()}
					});
			/*
			MetricCollection metricCollection = SchoolBusinessLayer.getInstance().getMetricCollectionDao().read(STUDENT_BEHAVIOUR_MERIC_COLLECTION_G1_G6);
			Collection<Metric> metrics = rootBusinessLayer.getMetricBusiness().findByCollection(metricCollection);
			Collection<StudentResultsMetricValue> studentResultsMetricValues = SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness()
					.findByStudentResults(((StudentClassroomSessionDivision)report.getSource()).getResults()); 
			report.addLabelValueCollection(metricCollection.getName() ,convertStudentResultsMetricValueToArray(metrics, studentResultsMetricValues));
			*/
			addStudentResultsLabelValueCollection(report, ((StudentClassroomSessionDivision)report.getSource()).getResults(), STUDENT_BEHAVIOUR_MERIC_COLLECTION_G1_G6);
			labelValueCollectionReport = new LabelValueCollectionReport();
			labelValueCollectionReport.setName(report.getCurrentLabelValueCollection().getName());
			labelValueCollectionReport.setCollection(report.getCurrentLabelValueCollection().getCollection().subList(6, 12));
			report.getCurrentLabelValueCollection().setCollection(report.getCurrentLabelValueCollection().getCollection().subList(0, 6));
			
			report.addLabelValueCollection(labelValueCollectionReport);
			
			IntervalCollection intervalCollection = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findCommonNodeInformations(
					((StudentClassroomSessionDivision)report.getSource()).getClassroomSessionDivision().getClassroomSession()).getStudentClassroomSessionDivisionAverageScale();
			String[][] values =  convertToArray(rootBusinessLayer.getIntervalBusiness().findByCollection(intervalCollection),Boolean.TRUE);
			commonUtils.swapColumns(values, 1, 2);
			report.addLabelValueCollection(intervalCollection.getName(),values);
			/*
			report.addLabelValueCollection("GRADING SCALE",new String[][]{
					{"A+", "Excellent","90 - 100"}
					,{"A",  "Very Good","80 - 89.99"}
					,{"B+", "Good","70 - 79.99"}
					,{"B",  "Fair","60 - 69.99"}
					,{"C+", "Satisfactory","55 - 59.99"}
					,{"C",  "Barely satisfactory","50 - 54.99"}
					,{"E",  "Fail","00 - 49.99"}
					});
			*/
			
			intervalCollection = SchoolBusinessLayer.getInstance().getMetricCollectionDao().read(STUDENT_BEHAVIOUR_MERIC_COLLECTION_G1_G6).getValueIntervalCollection();
			values =  convertToArray(rootBusinessLayer.getIntervalBusiness().findByCollection(intervalCollection),Boolean.FALSE);
			report.addLabelValueCollection(intervalCollection.getName(),values);
			
			/*
			report.addLabelValueCollection("EFFORT LEVELS",new String[][]{
					{"1", "Has no regard for the observable traits"}
					,{"2", "Shows minimal regard for the observable traits"}
					,{"3", "Acceptable level of observable traits"}
					,{"4", "Maintains high level of observable traits"}
					,{"5", "Maintains an excellent degree of observable traits"}
					});
			*/

			report.addLabelValueCollection("HOME/SCHOOL COMMUNICATIONS",new String[][]{
					{"ANNUAL AVERAGE","90"}
					,{"ANNUAL GRADE","B+"}
					,{"ANNUAL RANK","25"}
					,{"PROMOTION INFORMATION","PROMOTED"}
					,{"NEXT ACADEMIC YEAR","7Th SEPTEMBER 2015"}
					});
			
			return report;
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
