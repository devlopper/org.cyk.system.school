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

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.api.structure.CompanyBusiness;
import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.company.model.structure.Company;
import org.cyk.system.root.business.api.BusinessService.BusinessServiceCallArguments;
import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.mathematics.MetricCollectionIdentifiableGlobalIdentifierBusiness;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.root.business.impl.party.ApplicationBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.report.ReportTemplate;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.mathematics.MetricCollectionIdentifiableGlobalIdentifier;
import org.cyk.system.root.model.security.Installation;
import org.cyk.system.root.persistence.api.GenericDao;
import org.cyk.system.root.persistence.api.file.FileDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalCollectionDao;
import org.cyk.system.root.persistence.api.mathematics.IntervalDao;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper.ClassroomSessionInfos;
import org.cyk.system.school.business.impl.integration.AbstractSchoolFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.session.LevelGroupDao;
import org.cyk.system.school.persistence.api.session.LevelTimeDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.cdi.AbstractBean;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.joda.time.DateTime;
import org.joda.time.DateTimeConstants;

import lombok.Getter;
import lombok.Setter;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractSchoolFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;
		
	public static final String REPORT_CYK_GLOBAL_RANKABLE = "CYK_GLOBAL_RANKABLE";
	
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
	
	//private ClassroomSessionInfos pk,k1,k2,k3,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12;
	private MetricCollection[] pkMetricCollections,k1MetricCollections,k2k3MetricCollections
	,g1g6MetricCollections,g7g12MetricCollections,attendanceMetricCollections;
	
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
		PersistDataListener.COLLECTION.add(new PersistDataListener.Adapter.Default(){
			private static final long serialVersionUID = -950053441831528010L;
			@SuppressWarnings("unchecked")
			@Override
			public <T> T processPropertyValue(Class<?> aClass,String instanceCode, String name, T value) {
				if(File.class.equals(aClass)){
					
					if(CompanyConstant.FILE_DOCUMENT_HEADER.equals(instanceCode)){
						if(PersistDataListener.BASE_PACKAGE.equals(name))
							return (T) SchoolBusinessLayer.class.getPackage();
						if(PersistDataListener.RELATIVE_PATH.equals(name))
							return (T) "image/iesa/document_header.png";
					}else if(CompanyConstant.FILE_DOCUMENT_BACKGROUND.equals(instanceCode)){
						if(PersistDataListener.BASE_PACKAGE.equals(name))
							return (T) SchoolBusinessLayer.class.getPackage();
						if(PersistDataListener.RELATIVE_PATH.equals(name))
							return (T) "image/iesa/document_background.jpg";
					}else if(CompanyConstant.FILE_DOCUMENT_BACKGROUND_DRAFT.equals(instanceCode)){
						if(PersistDataListener.BASE_PACKAGE.equals(name))
							return (T) SchoolBusinessLayer.class.getPackage();
						if(PersistDataListener.RELATIVE_PATH.equals(name))
							return (T) "image/iesa/document_background_draft.jpg";
					}
					
					if(PersistDataListener.BASE_PACKAGE.equals(name))
						if(StringUtils.startsWith(instanceCode, "Iesa"))
							return (T) SchoolBusinessLayer.class.getPackage();
					
					
				}
				
				return super.processPropertyValue(aClass, instanceCode, name, value);
			}
		});
		
		
		AbstractIdentifiableBusinessServiceImpl.addAutoSetPropertyValueClass(new String[]{GlobalIdentifier.FIELD_CODE,GlobalIdentifier.FIELD_NAME}
		,AcademicSession.class,Level.class,LevelTimeDivision.class,ClassroomSession.class,ClassroomSessionDivision.class,ClassroomSessionDivisionSubject.class
		,ClassroomSessionDivisionSubjectEvaluationType.class,StudentClassroomSession.class,StudentClassroomSessionDivision.class
		,StudentClassroomSessionDivisionSubject.class);
		
		ApplicationBusinessImpl.Listener.COLLECTION.add(new ApplicationBusinessImpl.Listener.Adapter.Default(){
			private static final long serialVersionUID = 6894726061444433277L;

			@Override
			public void installationStarted(Installation installation) {
				super.installationStarted(installation);
				installation.getApplication().setName("IESA WebApp");
			}
		});
		
		CompanyBusinessLayer.Listener.COLLECTION.add(new CompanyBusinessLayer.Listener.Adapter() {
			private static final long serialVersionUID = 5179809445850168706L;

			@Override
			public String getCompanyName() {
				return "IESA";
			}
			
			@Override
			public byte[] getCompanyLogoBytes() {
				return getResourceAsBytes(SchoolBusinessLayer.class.getPackage(),"image/iesa/logo.png");
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
    			if(classroomSessionDivision.getOrderNumber()==1){
					classroomSessionDivision.getExistencePeriod().getNumberOfMillisecond().set(63l * DateTimeConstants.MILLIS_PER_DAY);
				}else if(classroomSessionDivision.getOrderNumber()==2){
					classroomSessionDivision.getExistencePeriod().setFromDate(new DateTime(2017, 1, 9, 0, 0).toDate());
	    			classroomSessionDivision.getExistencePeriod().setToDate(new DateTime(2017, 3, 27, 0, 0).toDate());
				}
    			classroomSessionDivision.setStudentSubjectAttendanceAggregated(Boolean.FALSE);
    		}
			
			@Override
			public void classroomSessionDivisionSubjectEvaluationTypeCreated(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
				super.classroomSessionDivisionSubjectEvaluationTypeCreated(classroomSessionDivisionSubjectEvaluationType);
				classroomSessionDivisionSubjectEvaluationType.setMaximumValue(new BigDecimal("100"));
				classroomSessionDivisionSubjectEvaluationType.setCountInterval(inject(IntervalDao.class).read(SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE));
			}
    	});
		
		/*org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionBusinessImpl.Listener.COLLECTION.add(
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
					
					
				});*/
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void structure(){
		LevelGroup levelGroupKindergarten = inject(LevelGroupDao.class).read(SchoolConstant.Code.LevelGroup.KINDERGARTEN);
		LevelGroup levelGroupPrimaryLower = inject(LevelGroupDao.class).read(SchoolConstant.Code.LevelGroup.PRIMARY_LOWER);
		LevelGroup levelGroupPrimaryHigher = inject(LevelGroupDao.class).read(SchoolConstant.Code.LevelGroup.PRIMARY_HIGHER);
		LevelGroup levelGroupSecondaryLower = inject(LevelGroupDao.class).read(SchoolConstant.Code.LevelGroup.SECONDARY_LOWER);
		LevelGroup levelGroupSecondaryHigher = inject(LevelGroupDao.class).read(SchoolConstant.Code.LevelGroup.SECONDARY_HIGHER);
		
		// Subjects
		schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.MATHEMATICS),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.GRAMMAR),new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.READING_COMPREHENSION),new ArrayList[]{subjectsG1G3});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.HANDWRITING),new ArrayList[]{subjectsG1G3});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.SPELLING),new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.PHONICS),new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.CREATIVE_WRITING),new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.MORAL_EDUCATION),new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.SOCIAL_STUDIES),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.SCIENCE),new ArrayList[]{subjectsG1G3,subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.FRENCH),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.ART_CRAFT),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.MUSIC),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.ICT_COMPUTER),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.PHYSICAL_EDUCATION),new ArrayList[]{subjectsG1G3,subjectsG4G6,subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.LITERATURE),new ArrayList[]{subjectsG1G3});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.COMPREHENSION),new ArrayList[]{subjectsG1G3});
    	
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.LITERATURE),new ArrayList[]{subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.COMPREHENSION),new ArrayList[]{subjectsG4G6});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.HISTORY),new ArrayList[]{subjectsG4G6,subjectsG7G9,subjectsG10G12});
    	
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.ENGLISH_FIRST_LANGUAGE),new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH),new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.GEOGRAPHY),new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.PHYSICS),new ArrayList[]{subjectsG7G9,subjectsG10G12});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.CHEMISTRY),new ArrayList[]{subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.BIOLOGY),new ArrayList[]{subjectsG7G9});
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.SPANISH),new ArrayList[]{subjectsG7G9});
    	
    	schoolDataProducerHelper.addSubjects(Arrays.asList(SchoolConstant.Code.Subject.SOCIOLOGY),new ArrayList[]{subjectsG10G12});
    	
		//Evaluation Type
    	evaluationTypes.add(evaluationTypeTest1 = inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST1));
		evaluationTypes.add(evaluationTypeTest2 = inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST2));
		evaluationTypes.add(evaluationTypeExam = inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.EXAM));
				
    	File documentHeaderFile = inject(FileDao.class).read(CompanyConstant.FILE_DOCUMENT_HEADER); 
    	File documentBackgroundImageFile = inject(FileDao.class).read(CompanyConstant.FILE_DOCUMENT_BACKGROUND); 
    	File documentBackgroundImageDraftFile = inject(FileDao.class).read(CompanyConstant.FILE_DOCUMENT_BACKGROUND_DRAFT); 
    	
    	ReportTemplate reportTemplatePk = rootDataProducerHelper.createReportTemplate("IesaReportTemplatePK","Report Sheet",Boolean.TRUE,"report/iesa/pkg.jrxml",documentHeaderFile
				, documentBackgroundImageFile, documentBackgroundImageDraftFile);
    	
    	ReportTemplate reportTemplateK1 = rootDataProducerHelper.createReportTemplate("IesaReportTemplateK1","Report Sheet",Boolean.TRUE,"report/iesa/kg1.jrxml",documentHeaderFile
				, documentBackgroundImageFile, documentBackgroundImageDraftFile);
    	
    	ReportTemplate reportTemplateK2K3 = rootDataProducerHelper.createReportTemplate("IesaReportTemplateK2K3","Report Sheet",Boolean.TRUE,"report/iesa/kg2kg3.jrxml",documentHeaderFile
				, documentBackgroundImageFile, documentBackgroundImageDraftFile);
    	
		ReportTemplate reportTemplateG1G12 = rootDataProducerHelper.createReportTemplate("IesaReportTemplateG1G12", "Report Sheet"
				, Boolean.TRUE, "report/iesa/g1g12.jrxml", documentHeaderFile, documentBackgroundImageFile, documentBackgroundImageDraftFile); 
		
		String classroomSessionDivisionIndex = "1";
		CommonNodeInformations commonNodeInformationsPk = schoolDataProducerHelper.instanciateOneCommonNodeInformations(null,null, reportTemplatePk, RootConstant.Code.TimeDivisionType.DAY
				, RootConstant.Code.TimeDivisionType.TRIMESTER,"50", classroomSessionDivisionIndex);
		
		CommonNodeInformations commonNodeInformationsK1 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(null,null, reportTemplateK1, RootConstant.Code.TimeDivisionType.DAY
				, RootConstant.Code.TimeDivisionType.TRIMESTER,"50", classroomSessionDivisionIndex);
		
		CommonNodeInformations commonNodeInformationsK2K3 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(null,null, reportTemplateK2K3, RootConstant.Code.TimeDivisionType.DAY
				, RootConstant.Code.TimeDivisionType.TRIMESTER,"50", classroomSessionDivisionIndex);
		
		CommonNodeInformations commonNodeInformationsG1G3 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(inject(IntervalCollectionDao.class)
				.read(SchoolConstant.Code.IntervalCollection.GRADING_SCALE_PRIMARY_STUDENT),inject(IntervalCollectionDao.class)
				.read(SchoolConstant.Code.IntervalCollection.PROMOTION_SCALE_STUDENT),reportTemplateG1G12,RootConstant.Code.TimeDivisionType.DAY, RootConstant.Code.TimeDivisionType.TRIMESTER, "50", classroomSessionDivisionIndex);
		CommonNodeInformations commonNodeInformationsG4G6 = commonNodeInformationsG1G3;
		
		CommonNodeInformations commonNodeInformationsG7G9 = schoolDataProducerHelper.instanciateOneCommonNodeInformations(inject(IntervalCollectionDao.class)
				.read(SchoolConstant.Code.IntervalCollection.GRADING_SCALE_SECONDARY_STUDENT),inject(IntervalCollectionDao.class)
				.read(SchoolConstant.Code.IntervalCollection.PROMOTION_SCALE_STUDENT),reportTemplateG1G12,RootConstant.Code.TimeDivisionType.DAY, RootConstant.Code.TimeDivisionType.TRIMESTER, "50", classroomSessionDivisionIndex);	
		CommonNodeInformations commonNodeInformationsG10G12 = commonNodeInformationsG7G9;
		
    	createMetricCollections();
    	
    	inject(GenericBusiness.class).create(commonUtils.castCollection(inject(TeacherBusiness.class).instanciateManyRandomly(numbreOfTeachers),AbstractIdentifiable.class));
		flush("Teachers");
		inject(GenericBusiness.class).create(commonUtils.castCollection(inject(StudentBusiness.class).instanciateManyRandomly(numbreOfStudents),AbstractIdentifiable.class));
		flush("Students");
		
    	//school.getOwnedCompany().getCompany().setManager(personDao.readOneRandomly());
    	//companyBusiness.update(school.getOwnedCompany().getCompany());
    	    	
    	Collection<ClassroomSession> classroomSessions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivision> classroomSessionDivisions = new ArrayList<>(); 
    	Collection<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjects = new ArrayList<>();
    	Collection<ClassroomSessionDivisionSubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>(); 
    	Collection<MetricCollectionIdentifiableGlobalIdentifier> metricCollectionIdentifiableGlobalIdentifiers = new ArrayList<>(); 
    	
    	Long gradeIndex = 0l;
    	/*
    	pk = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes
    			,metricCollectionIdentifiableGlobalIdentifiers,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision(SchoolConstant.Code.LevelName.PK,"Pre-Kindergarten",levelGroupKindergarten,commonNodeInformationsPk,gradeIndex++) 
    			,null,null,null,null,new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS
    					,SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT},Boolean.FALSE,Boolean.FALSE).iterator().next();
    	*/
    	/*
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,metricCollectionIdentifiableGlobalIdentifiers,academicSession
    			,inject(LevelTimeDivisionDao.class).read(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1) 
    			,null,null,null,null,new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS
    					,SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT},Boolean.FALSE,Boolean.FALSE);
    	*/
    	/*
    	k2 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes
    			,metricCollectionIdentifiableGlobalIdentifiers,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision(SchoolConstant.Code.LevelName.K2,"Kindergarten 2",levelGroupKindergarten,commonNodeInformationsK2K3,gradeIndex++) 
    			,null,null,null,null,new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS
    					,SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT},Boolean.FALSE,Boolean.FALSE).iterator().next();
    	
    	k3 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes
    			,metricCollectionIdentifiableGlobalIdentifiers,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision(SchoolConstant.Code.LevelName.K3,"Kindergarten 3",levelGroupKindergarten,commonNodeInformationsK2K3,gradeIndex++) 
    			,null,null,null,null,new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION
    					,SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS
    					,SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT},Boolean.FALSE,Boolean.FALSE).iterator().next();
    	*/
    	/*
    	schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,metricCollectionIdentifiableGlobalIdentifiers,academicSession
    			, inject(GenericDao.class).read(LevelTimeDivision.class, SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1),null 
    			,new Object[][]{{evaluationTypeTest1,"0.15","100"},{evaluationTypeTest2,"0.15","100"},{evaluationTypeExam,"0.7","100"}}, subjectsG1G3
    			,new String[]{"A","B"},new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT,SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT
    					,SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT},Boolean.TRUE,Boolean.TRUE);  
    	
    	*/
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
    	*//*g12 = schoolDataProducerHelper.instanciateOneClassroomSession(classroomSessions,classroomSessionDivisions,classroomSessionDivisionSubjects,subjectEvaluationTypes,academicSession
    			, schoolDataProducerHelper.createLevelTimeDivision("G12","Grade 12",levelGroupSecondary,commonNodeInformationsG10G12,gradeIndex++) ,null
    			,new Object[][]{{evaluationTypeTest1,"0.2","100"},{evaluationTypeTest2,"0.2","100"},{evaluationTypeExam,"0.6","100"}},subjectsG7G9,classroomSessionDivisionStudentsMetricCollections
    			,g7g12MetricCollections,attendanceMetricCollections,new String[]{"A","B"},Boolean.TRUE,Boolean.FALSE).iterator().next();
    	*/
    	
    	flush(ClassroomSession.class, classroomSessionBusiness, classroomSessions);
    	flush(ClassroomSessionDivision.class, classroomSessionDivisionBusiness, classroomSessionDivisions);
    	flush(ClassroomSessionDivisionSubject.class, classroomSessionDivisionSubjectBusiness, classroomSessionDivisionSubjects);
    	flush(ClassroomSessionDivisionSubjectEvaluationType.class, subjectEvaluationTypeBusiness, subjectEvaluationTypes);
    	flush(MetricCollectionIdentifiableGlobalIdentifier.class, inject(MetricCollectionIdentifiableGlobalIdentifierBusiness.class)
    			, metricCollectionIdentifiableGlobalIdentifiers);
	}
	
	@Override
	public void produce(Listener listener) {
		this.listener =listener;
		rootDataProducerHelper.setBasePackage(SchoolBusinessLayer.class.getPackage());
		SchoolConstant.Code.EvaluationType.COLLECTION.addAll(Arrays.asList(SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.EvaluationType.TEST2
				,SchoolConstant.Code.EvaluationType.EXAM));
    	StudentClassroomSessionDivisionBusiness.SUM_MARKS[0] = Boolean.TRUE;
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
		/*for(ClassroomSessionInfos classroomSessionInfos : new ClassroomSessionInfos[]{g1,g2,g3,g4,g5,g6,g7,g8,g9}){
			Collection<Student> students = studentDao.readManyRandomly(numbreOfStudentsByClassroomSession);
			createStudentClassroomSessions(classroomSessionInfos, students);	
			//executor.execute(new ClassroomsessionBusinessProducer(classroomSessionInfos, listener, students,studentSubjects));
		}*/
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
	
	private void createMetricCollections(){/*
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
		
		attendanceMetricCollections = new MetricCollection[]{ create(inject(MetricCollectionBusiness.class).instanciateOne(RandomStringUtils.randomAlphanumeric(6),"Attendance",MetricValueType.NUMBER
    			, new String[]{"Attended","Absent"}
    	,"Values", new String[][]{ {"E", "Excellent", "0", "1000"} }).setMetricValueInputted(MetricValueInputted.VALUE_INTERVAL_VALUE))};
		*/
		/*
		create(inject(MetricCollectionBusiness.class).instanciateOne(MERIC_COLLECTION_STUDENT_ATTENDANCE,"SCHOOL ATTENDANCE"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.ATTENDANCE_STUDENT),MetricValueType.NUMBER
			, new String[]{"Number of time present","Number of time absent","Number of time on detention","Number of time suspended"}
    		,null, null));
		
		create(inject(MetricCollectionBusiness.class).instanciateOne(MERIC_COLLECTION_G1_G6_STUDENT_BEHAVIOUR,"Behaviour,Study and Work Habits"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies","Demonstrates self-control"
    		,"Takes care of school and others materials","Game/Sport","Handwriting","Drawing/Painting","Punctionality/Regularity","Works cooperatively in groups"
    		,"Listens and follows directions"}
    		,"Effort Levels", new String[][]{ {"1", "Has no regard for the observable traits", "1", "1"},{"2", "Shows minimal regard for the observable traits", "2", "2"}
    		,{"3", "Acceptable level of observable traits", "3", "3"},{"4", "Maintains high level of observable traits", "4", "4"}
    		,{"5", "Maintains an excellent degree of observable traits", "5", "5"} }));
		
		create(inject(MetricCollectionBusiness.class).instanciateOne(MERIC_COLLECTION_G7_G12_STUDENT_BEHAVIOUR,"Behaviour,Study and Work Habits"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.STRING
			, new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies","Demonstrates self-control"
    	    ,"Takes care of school and others materials","Game/Sport","Handwriting","Drawing/Painting","Punctionality/Regularity","Works cooperatively in groups"
    	    ,"Listens and follows directions"}
    	    ,"Effort Levels", new String[][]{ {"E", "Excellent", "1", "1"},{"G", "Good", "2", "2"},{"S", "Satisfactory", "3", "3"},{"N", "Needs Improvement", "4", "4"}
    	    ,{"H", "Has no regard", "5", "5"} }).setMetricValueInputted(MetricValueInputted.VALUE_INTERVAL_CODE));
		*/
	}

}
