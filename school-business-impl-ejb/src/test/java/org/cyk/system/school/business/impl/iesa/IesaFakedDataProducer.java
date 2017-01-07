package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.inject.Singleton;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.company.model.structure.Company;
import org.cyk.system.root.business.impl.AbstractIdentifiableBusinessServiceImpl;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.root.business.impl.party.ApplicationBusinessImpl;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.security.Installation;
import org.cyk.system.root.persistence.api.mathematics.IntervalDao;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolDataProducerHelper;
import org.cyk.system.school.business.impl.integration.AbstractSchoolFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.subject.EvaluationTypeDao;
import org.joda.time.DateTime;
import org.joda.time.DateTimeConstants;

import lombok.Getter;
import lombok.Setter;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractSchoolFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;
		
	@Setter private Integer numbreOfTeachers = 0;
	@Setter private Integer numbreOfStudents = 0;
	@Setter private Integer numbreOfLecturesByClassroomSessionDivisionSubject = 5;
	@Setter private Integer numbreOfStudentsByClassroomSession = 25;
	
	@Setter private Boolean generateCompleteAcademicSession = Boolean.FALSE;
	@Setter private Boolean generateStudentClassroomSessionDivisionReport = Boolean.FALSE;
	@Setter private Integer classroomSessionDivisionIndex = 1;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		PersistDataListener.COLLECTION.add(new PersistDataListener.Adapter.Default(){
			private static final long serialVersionUID = -950053441831528010L;
			@SuppressWarnings("unchecked")
			@Override
			public <T> T processPropertyValue(Class<?> aClass,String instanceCode, String name, T value) {
				if(File.class.equals(aClass)){
					
					if(CompanyConstant.Code.File.DOCUMENT_HEADER.equals(instanceCode)){
						if(PersistDataListener.BASE_PACKAGE.equals(name))
							return (T) SchoolBusinessLayer.class.getPackage();
						if(PersistDataListener.RELATIVE_PATH.equals(name))
							return (T) "image/iesa/document_header.png";
					}else if(CompanyConstant.Code.File.DOCUMENT_BACKGROUND.equals(instanceCode)){
						if(PersistDataListener.BASE_PACKAGE.equals(name))
							return (T) SchoolBusinessLayer.class.getPackage();
						if(PersistDataListener.RELATIVE_PATH.equals(name))
							return (T) "image/iesa/document_background.jpg";
					}else if(CompanyConstant.Code.File.DOCUMENT_BACKGROUND_DRAFT.equals(instanceCode)){
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
			
			/*@Override
			public byte[] getCompanyLogoBytes() {
				return getResourceAsBytes(SchoolBusinessLayer.class.getPackage(),"image/iesa/logo.png");
			}*/
			
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
		
	}
	
	@Override
	protected void structure(){
		inject(EvaluationTypeDao.class).delete(inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST));
		// Subjects
		/*
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
    	*/
		//Evaluation Type
    	/*
    	evaluationTypes.add(evaluationTypeTest1 = inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST1));
		evaluationTypes.add(evaluationTypeTest2 = inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.TEST2));
		evaluationTypes.add(evaluationTypeExam = inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.EXAM));
		*/
		/*		
    	File documentHeaderFile = inject(FileDao.class).read(CompanyConstant.Code.File.DOCUMENT_HEADER); 
    	File documentBackgroundImageFile = inject(FileDao.class).read(CompanyConstant.Code.File.DOCUMENT_BACKGROUND); 
    	File documentBackgroundImageDraftFile = inject(FileDao.class).read(CompanyConstant.Code.File.DOCUMENT_BACKGROUND_DRAFT); 
    	
    	ReportTemplate reportTemplatePk = rootDataProducerHelper.createReportTemplate("IesaReportTemplatePK","Report Sheet",Boolean.TRUE,"report/iesa/pkg.jrxml",documentHeaderFile
				, documentBackgroundImageFile, documentBackgroundImageDraftFile);
    	
    	ReportTemplate reportTemplateK1 = rootDataProducerHelper.createReportTemplate("IesaReportTemplateK1","Report Sheet",Boolean.TRUE,"report/iesa/kg1.jrxml",documentHeaderFile
				, documentBackgroundImageFile, documentBackgroundImageDraftFile);
    	
    	ReportTemplate reportTemplateK2K3 = rootDataProducerHelper.createReportTemplate("IesaReportTemplateK2K3","Report Sheet",Boolean.TRUE,"report/iesa/kg2kg3.jrxml",documentHeaderFile
				, documentBackgroundImageFile, documentBackgroundImageDraftFile);
    	
		ReportTemplate reportTemplateG1G12 = rootDataProducerHelper.createReportTemplate("IesaReportTemplateG1G12", "Report Sheet"
				, Boolean.TRUE, "report/iesa/g1g12.jrxml", documentHeaderFile, documentBackgroundImageFile, documentBackgroundImageDraftFile); 
		*/		
    	
		
    	//school.getOwnedCompany().getCompany().setManager(personDao.readOneRandomly());
    	//companyBusiness.update(school.getOwnedCompany().getCompany());
    	    	
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
    	   	
	}
	
	@Override
	public void produce(Listener listener) {
		this.listener =listener;
		rootDataProducerHelper.setBasePackage(SchoolBusinessLayer.class.getPackage());
		
		SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT = Boolean.TRUE;
		
    	structure();
    	
    	if(Boolean.TRUE.equals(generateCompleteAcademicSession)){
    		doBusiness(listener);
    	}
	}
	
	@Override
	protected void doBusiness(Listener listener){
		
	}

	/**/
		
	
}
