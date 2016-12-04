package org.cyk.system.school.business.impl;

import java.io.Serializable;

import javax.inject.Singleton;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.CompanyDataProducerHelper;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.company.model.structure.Employee;
import org.cyk.system.root.business.api.ClazzBusiness;
import org.cyk.system.root.business.api.FormatterBusiness;
import org.cyk.system.root.business.api.mathematics.IntervalCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.RankOptions.RankType;
import org.cyk.system.root.business.api.mathematics.MetricCollectionBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.root.business.api.mathematics.NumberBusiness.FormatArguments;
import org.cyk.system.root.business.api.network.UniformResourceLocatorBusiness;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.api.time.TimeBusiness;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFormatter;
import org.cyk.system.root.business.impl.BusinessServiceProvider;
import org.cyk.system.root.business.impl.BusinessServiceProvider.Service;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.business.impl.file.report.AbstractRootReportProducer;
import org.cyk.system.root.business.impl.party.person.AbstractActorBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.ContentType;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollectionType;
import org.cyk.system.root.model.mathematics.MetricValueInputted;
import org.cyk.system.root.model.mathematics.MetricValueType;
import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.root.model.security.Role;
import org.cyk.system.root.persistence.api.mathematics.MetricCollectionTypeDao;
import org.cyk.system.school.business.api.SortableStudentResults;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.actor.TeacherBusinessImpl;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.SubjectClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.actor.StudentDao;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;
import org.cyk.utility.common.computation.DataReadConfiguration;

import lombok.Getter;
import lombok.Setter;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER) @Getter
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	private static SchoolBusinessLayer INSTANCE;
	
	@Setter private AverageComputationListener averageComputationListener;
	@Setter private Script averageComputationScript;
	
	private String actionCreateSubjectEvaluation = "acse";
	private String actionUpdateStudentClassroomSessionDivisionResults = "auscsdr";
	private String actionComputeStudentClassroomSessionDivisionEvaluationResults = "acscsder";
	private String actionComputeStudentClassroomSessionEvaluationResults = "acscser";
	private String actionComputeStudentClassroomSessionDivisionRankResults = "acscsdrr";
	private String actionComputeStudentClassroomSessionDivisionAttendanceResults = "acscsdar";
	private String actionUpdateStudentClassroomSessionDivisionReportFiles = "auscsdrf";
	private String actionSendStudentClassroomSessionDivisionReportFiles = "asscsdrf";
	private String actionConsultStudentClassroomSessionDivisionReportFiles = "acscsdrf";
	private String actionConsultStudentClassroomSessionRanks = "acscsr";
	
	
	private String actionEditStudentClassroomSessionDivisionEvaluationAverage = "aescsdea";
	private String actionConsultClassroomSessionDivisionBroadsheet = "accsdbs";
	
	private RankOptions<SortableStudentResults> studentEvaluationResultsRankOptions = new RankOptions<>();
	
	@Override
	protected void initialisation() {
		INSTANCE = this;
		super.initialisation();
		PersistDataListener.COLLECTION.add(new PersistDataListener.Adapter.Default(){
			private static final long serialVersionUID = -950053441831528010L;
			@SuppressWarnings("unchecked")
			@Override
			public <T> T processPropertyValue(Class<?> aClass,String instanceCode, String name, T value) {
				if(ArrayUtils.contains(new String[]{CompanyConstant.REPORT_EMPLOYEE_EMPLOYMENT_CONTRACT}, instanceCode)){
					if(PersistDataListener.BASE_PACKAGE.equals(name))
						return (T) CompanyBusinessLayer.class.getPackage();
				}
				return super.processPropertyValue(aClass, instanceCode, name, value);
			}
		});
		
		AbstractRootReportProducer.DEFAULT = new AbstractSchoolReportProducer.Default();
		RootBusinessLayer.GLOBAL_IDENTIFIER_UNBUILDABLE_CLASSES.add(StudentResults.class);
		registerFormatter(AcademicSession.class, new AbstractFormatter<AcademicSession>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(AcademicSession academicSession, ContentType contentType) {
				return inject(TimeBusiness.class).formatDate(academicSession.getExistencePeriod().getFromDate(), academicSession.getExistencePeriod().getToDate(), TimeBusiness.DATE_SHORT_PATTERN) ;
			}
		});
		registerFormatter(ClassroomSession.class, new AbstractFormatter<ClassroomSession>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSession classroomSession, ContentType contentType) {
				return classroomSession.getLevelTimeDivision().getLevel().getLevelName().getName()
						+(StringUtils.isBlank(classroomSession.getSuffix())?Constant.EMPTY_STRING:Constant.CHARACTER_SPACE+classroomSession.getSuffix());
			}
		});
		registerFormatter(ClassroomSessionDivision.class, new AbstractFormatter<ClassroomSessionDivision>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(ClassroomSessionDivision classroomSessionDivision, ContentType contentType) {
				return classroomSessionDivision.getTimeDivisionType().getName()+Constant.CHARACTER_SPACE+(classroomSessionDivision.getOrderNumber());
			}
		});
		
		registerFormatter(StudentClassroomSessionDivision.class, new AbstractFormatter<StudentClassroomSessionDivision>() {
			private static final long serialVersionUID = -4793331650394948152L;
			@Override
			public String format(StudentClassroomSessionDivision studentClassroomSessionDivision, ContentType contentType) {
				return inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getClassroomSessionDivision(),contentType)+Constant.CHARACTER_SPACE
						+inject(FormatterBusiness.class).format(studentClassroomSessionDivision.getStudent(),contentType);
			}
		});
		
		ClazzBusiness.LISTENERS.add(new ClazzBusiness.ClazzBusinessListener.Adapter(){
			private static final long serialVersionUID = -6563167908087619179L;
			@Override
			public Object getParentOf(Object object) {
				if(object instanceof AbstractIdentifiable){
					AbstractIdentifiable identifiable = (AbstractIdentifiable) object;
					if(identifiable instanceof ClassroomSession)
						return null;//((ClassroomSession)object).getAcademicSession();
					if(identifiable instanceof SubjectClassroomSession)
						return ((SubjectClassroomSession)identifiable).getClassroomSession();
					
					if(identifiable instanceof ClassroomSessionDivision)
						return ((ClassroomSessionDivision)identifiable).getClassroomSession();
					if(identifiable instanceof ClassroomSessionDivisionSubject)
						return ((ClassroomSessionDivisionSubject)identifiable).getClassroomSessionDivision();
					if(identifiable instanceof ClassroomSessionDivisionSubjectEvaluationType)
						return ((ClassroomSessionDivisionSubjectEvaluationType)identifiable).getClassroomSessionDivisionSubject();
					
					if(identifiable instanceof Evaluation)
						return ((Evaluation)identifiable).getClassroomSessionDivisionSubjectEvaluationType();
					
					if(identifiable instanceof StudentClassroomSession)
						return ((StudentClassroomSession)identifiable).getClassroomSession();
					if(identifiable instanceof StudentClassroomSessionDivision)
						return ((StudentClassroomSessionDivision)identifiable).getClassroomSessionDivision();
					if(identifiable instanceof StudentClassroomSessionDivisionSubject)
						return ((StudentClassroomSessionDivisionSubject)identifiable).getClassroomSessionDivisionSubject();
				}
				return super.getParentOf(object);
			}
		});
		
		BusinessServiceProvider.Identifiable.COLLECTION.add(new AbstractActorBusinessImpl.BusinessServiceProviderIdentifiable<Student,Student.SearchCriteria>(Student.class){
			private static final long serialVersionUID = 1322416788278558869L;
			
			@Override
			protected Student.SearchCriteria createSearchCriteria(Service service,DataReadConfiguration dataReadConfiguration) {
				return new Student.SearchCriteria(dataReadConfiguration.getGlobalFilter());
			}
        });
		
		BusinessServiceProvider.Identifiable.COLLECTION.add(new AbstractActorBusinessImpl.BusinessServiceProviderIdentifiable<Teacher,Teacher.SearchCriteria>(Teacher.class){
			private static final long serialVersionUID = 1322416788278558869L;
			
			@Override
			protected Teacher.SearchCriteria createSearchCriteria(Service service,DataReadConfiguration dataReadConfiguration) {
				return new Teacher.SearchCriteria(dataReadConfiguration.getGlobalFilter());
			}
        });
		
		studentEvaluationResultsRankOptions.setType(RankType.EXAEQUO);
		studentEvaluationResultsRankOptions.getSortOptions().setComparator(new SortableStudentResultsComparator(Boolean.TRUE));
	}
	
	@Override
	protected AbstractReportRepository getReportRepository() {
		return inject(SchoolReportRepository.class);
	}
	
	@Override
	protected void persistData() {
		//create(inject(IntangibleProductBusiness.class).instanciateOne(SchoolConstant.INTANGIBLE_PRODUCT_TUITION));
    	//create(new SalableProduct(getEnumeration(IntangibleProduct.class, SchoolConstant.INTANGIBLE_PRODUCT_TUITION), null));
    	
    	inject(CompanyDataProducerHelper.class).createReportTemplate(SchoolConstant.REPORT_STUDENT_REGISTRATION_CERTIFICATE,"certificat d'inscription",Boolean.TRUE, "report/student/registration_certificate.jrxml");
    	inject(CompanyDataProducerHelper.class).createReportTemplate(SchoolConstant.REPORT_STUDENT_TUITION_CERTIFICATE,"certificat de scolarité",Boolean.TRUE, "report/student/tuition_certificate.jrxml");
    	//inject(CompanyDataProducerHelper.class).createReportTemplate(SchoolConstant.REPORT_STUDENT_CLASSROOM_SESSION_DIVISION_SHEET,"bulletin trimestriel",Boolean.TRUE, "report/student/classroom_session_division_sheet.jrxml");
    	
    	metricColletions();
    	
	}
	
	//TODO labels must be changed in french
	private void metricColletions(){
		String[] metricsCommon = null;
		String notAssessed = "Not Assessed",notAssessedAbbreviation = "NA";
		createEnumerations(MetricCollectionType.class,SchoolConstant.Code.MetricCollectionType.ATTENDANCE_STUDENT,SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT
    			,SchoolConstant.Code.MetricCollectionType.COMMUNICATION_STUDENT,SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_KINDERGARTEN_STUDENT
    			,SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_PRIMARY_STUDENT,SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_SECONDARY_STUDENT);
		
		IntervalCollection intervalCollection = create(inject(IntervalCollectionBusiness.class).instanciateOne(SchoolConstant.Code.IntervalCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT
				,"Skills performance levels",Constant.CHARACTER_UNDESCORE.toString(),new String[][]{ {"1", "Learning to do", "1", "1"},{"2", "Does sometimes", "2", "2"}
				,{"3", "Does regulary", "3", "3"} }));
		
		//PK
		
		create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE
    			,"Expressive language",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Participates actively during circle time","Participates in singing rhymes","Can say her name and name of classmates"
					,"Can respond appropriately to “how are you?”","Can say his/her age","Can say the name of her school","Names objects in the classroom and school environment"
					,"Uses at least one of the following words “me”,“I”, “he”, “she”, “you”","Talks in two or three word phrases and longer sentences"
					,"Can use “and” to connect words/phrases","Talks with words in correct order","Can be engaged in conversations"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE
    			,"Receptive language",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Responds to her name when called","Retrieves named objects","Follows simple instructions (across the classroom) – stand, sit, bring your cup"
					,"Understands facial expressions and tone of voice","Understands 2-3 step instructions"
					,"Understands positional words – In and out - Up and down - On and under - Forward and backward","Understands the concept “Give and Take”"
					,"Talks about feelings"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS
    			,"Reading readness",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Shows interest in book/stories","Names familiar objects in pictures/books – vegetables, fruits, animals","Tells what action is going on in pictures"
					,"Handling books – carrying a book, turning the pages of a book, placing a book back in the shelf","Listening for different sounds in the environment"
					,"Identifying objects that begin with a particular sound","Identifying pictures that begin with a particular sound","Recognizes the written letters of the alphabet"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT
    			,"Numeracy development",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Sorts objects by shape","Sorts objects by size","Participates in reciting different counting rhymes, songs, stories and games","Verbally count forward to 10"
					,"Can count 1-10 objects","Identifies the written numerals 1-10","Reproducing Patterns","Identifies the 3 basic geometric shapes ( circle,triangle and square)"
					,"Identifies more shapes ( Star, diamond, heart,cross ,crescent)"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC
    			,"Arts and music",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Moves expressively to sounds and music – nodding, clapping, movement of body","Participates in musical activities"
					,"Hums or sing words of songs","Participates in role play","Shows satisfaction with completed work"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT
    			,"Social and emotional development",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Initiates interaction with adults","Initiates interaction with classmates","Participates in group activities"
					,"Takes turns during group activities","Greets people – hello and goodbye","Says “please” and “thank you”","Asks for help in doing things when needed"
					,"Shows sympathy, offers to help or helps others","Can express dissatisfaction and other emotions – body language or words"
					,"Responds to correction – stops the misbehaviour"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS
    			,"Gross motor skills",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Can run well without falling","Can kick a ball","Climbs up ladder and slides down slide without help","Walks up and down stairs unassisted"
					,"Can stand on one foot for a few seconds without support","Throws a ball into a basket from a short distance"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS
    			,"Fine motor skills",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Scribbles spontaneously","Can scribble to and from, in circular motions and in lines","Can place simple pieces in a puzzle board"
					,"Can build a tower of at least 3-5 blocks","Develops good pencil grip and control"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	//KG1
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING
    			,"English/language Arts/reading",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Reads independently with understanding","Comprehends a variety of texts","Applies a variety of strategies to comprehend printed tex"
					,"Reads to access and utilize information from written and electronic sources","Demonstrates understanding of letter-sound associations"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS
    			,"Communication skills",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Contributes ideas to discussions","Contributes ideas to discussions","Write for a variety of purposes","Writes well-organized compositions"
					,"Uses appropriate writing skills","Write legibly","Revises, edits and proofreads work"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE
    			,"Science",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Understands and applies scientific process","Understands and applies knowledge of key concepts"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES
    			,"Social studies",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Gathers and organizes information","Understands and applies knowledge of key concepts"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS
    			,"Mathematics",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Demonstrates understanding of number sense","Reads and interprets data","Applies problem-solving strategies"
					,"Communicates mathematically"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS
    			,"Work habits",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Follows directions","Uses time and materials constructively","Works independently","Completes class assignments"
					,"Completes homework assignments ","Listens attentively"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS
    			,"Social skills",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Cooperates with others","Shows respect for others","Participates in classroom activities","Follows classroom/school rules"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	//KG2 & KG3
    	
    	intervalCollection = create(inject(IntervalCollectionBusiness.class).instanciateOne(SchoolConstant.Code.IntervalCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT,"Skills performance levels"
				,Constant.CHARACTER_UNDESCORE.toString(),new String[][]{{"4","Meets and applies expectations/standards independently","4","4"}
				,{"3","Meets and applies expectations/standards with support","3","3"},{"2","Does not meets and applies expectations/standards; but shows growth with support","2","2"}
				,{"1","Does not meets and applies expectations/standards; shows no growth even with support","1","1"}}));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS
    			,"Reading Readiness",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Demonstrates concepts of print","Identifies and produces rhyming words","Segments and blends sounds"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING
    			,"Reading",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Answers questions about essential narrative elements","Reads high frequency words","Blends sounds to read words","Reads simple text"
					,"Developmental Reading assessment"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING
    			,"Writing",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Writes first and last name","Expresses ideas through independent writing"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING
    			,"Listening, speaking and viewing",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Uses oral language to communicate effectively","Recites short poems and songs","Follows two-step oral directions","Makes predictions and retells"
					,"Comprehends information through listening","Demonstrates comprehension of information through speaking"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION
    			,"Alphabet identification",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Identifies Upper-Case","Identifies Lower-Case","Produces Letter Sounds","Prints Letters Correctly"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS
    			,"Mathematics",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Number and Operations","Geometry","Measurement","Algebraic Thinking"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION
    			,"Science, social studies and moral education",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Science","Social Studies","Moral Education"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT
    			,"Art and craft",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Performance","Initiative"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));

    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC
    			,"Music",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Performance","Initiative"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION
    			,"Physical education",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Performance","Initiative"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS
    			,"Work and behaviour habits",inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, new String[]{"Follows directions","Uses time and materials constructively","Works independently","Completes class assignments","Completes homework assignments"
					,"Listens attentively","Cooperates with others","Shows respect for others","Participates in classroom activities","Follows classroom/school rules"}
    		,intervalCollection).setValueIsNullable(Boolean.TRUE).setNullValueString(notAssessed).setNullValueAbbreviation(notAssessedAbbreviation));
    	
    	// G1 - G12
    	
    	intervalCollection = create(inject(IntervalCollectionBusiness.class).instanciateOne(SchoolConstant.Code.IntervalCollection.BEHAVIOUR_PRIMARY_STUDENT,"Effort Levels"
				,Constant.CHARACTER_UNDESCORE.toString(),new String[][]{ {"1", "Has no regard for the observable traits", "1", "1"},{"2", "Shows minimal regard for the observable traits"
					, "2", "2"},{"3", "Acceptable level of observable traits", "3", "3"},{"4", "Maintains high level of observable traits", "4", "4"}
					,{"5", "Maintains an excellent degree of observable traits", "5", "5"} }));
    	
    	String[] items = new String[]{"Respect authority","Works independently and neatly","Completes homework and class work on time","Shows social courtesies"
    			,"Demonstrates self-control","Takes care of school and others materials","Event management skills","Game/Sport","Handwriting","Drawing/Painting"
    			,"Punctionality/Regularity","Works cooperatively in groups","Listens and follows directions","Community"};
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT,"Behaviour,Study and Work Habits"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.NUMBER
			, items,intervalCollection));
		
    	intervalCollection = create(inject(IntervalCollectionBusiness.class).instanciateOne(SchoolConstant.Code.IntervalCollection.BEHAVIOUR_SECONDARY_STUDENT,"Effort Levels"
				,Constant.CHARACTER_UNDESCORE.toString(),new String[][]{ {"E", "Excellent", "1", "1"},{"G", "Good", "2", "2"},{"S", "Satisfactory", "3", "3"}
				,{"N", "Needs Improvement", "4", "4"},{"H", "Has no regard", "5", "5"} }));
    	
		create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT,"Behaviour,Study and Work Habits"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.BEHAVIOUR_STUDENT),MetricValueType.STRING
			, items, intervalCollection).setMetricValueInputted(MetricValueInputted.VALUE_INTERVAL_CODE));
		
		metricsCommon = new String[]{"Conference requested"};
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT,"School communications"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.COMMUNICATION_STUDENT),MetricValueType.BOOLEAN
			, ArrayUtils.addAll(metricsCommon),null, null));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT,"School communications"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.COMMUNICATION_STUDENT),MetricValueType.BOOLEAN
			, ArrayUtils.addAll(metricsCommon,"Promotion in danger"),null, null));
    	
    	metricsCommon = new String[]{"Number of time present","Number of time absent"};
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT,"School attendance"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.ATTENDANCE_STUDENT),MetricValueType.NUMBER
			, ArrayUtils.addAll(metricsCommon,"Number of time on detention","Number of time suspended"),null, null));
    	
    	create(inject(MetricCollectionBusiness.class).instanciateOne(SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT,"School attendance"
				,inject(MetricCollectionTypeDao.class).read(SchoolConstant.Code.MetricCollectionType.ATTENDANCE_STUDENT),MetricValueType.NUMBER, metricsCommon,null, null));
	}
	
	
	@Override
	protected void persistSecurityData(){
		UniformResourceLocatorBusiness uniformResourceLocatorBusiness = inject(UniformResourceLocatorBusiness.class);
		Role userRole = getEnumeration(Role.class,Role.USER);
		Role managerRole = getEnumeration(Role.class,Role.MANAGER);
		Role teacherRole = create(new Role(SchoolConstant.Code.Role.TEACHER, "Teacher"));
		
		instanciateRoleUniformResourceLocator(userRole, uniformResourceLocatorBusiness.instanciateOne("/private/index.jsf",new String[]{})
				,uniformResourceLocatorBusiness.instanciateOne("/private/file/consultmany.jsf",new String[]{})
				,uniformResourceLocatorBusiness.instanciateOne("/private/classroomsessiondivision/consult.jsf",new String[]{UniformResourceLocatorParameter.TAB_ID,"school.broadsheet" }));
		
		instanciateRoleUniformResourceLocator(managerRole,uniformResourceLocatorBusiness.instanciateManyCrud(Student.class)
				,uniformResourceLocatorBusiness.instanciateManyCrud(Teacher.class)
				,uniformResourceLocatorBusiness.instanciateManyCrud(Employee.class)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSession.class, Boolean.TRUE, Boolean.TRUE, Boolean.TRUE,null, null, new String[]{"auscsdrf","acscsdrf"})
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSessionDivision.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSessionDivisionSubject.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(ClassroomSessionDivisionSubjectEvaluationType.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, new String[]{"acse","auscsdr"}, null)
				);
		
		instanciateRoleUniformResourceLocator(teacherRole
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(StudentClassroomSession.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,Boolean.TRUE, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(StudentClassroomSessionDivision.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(StudentClassroomSessionDivisionSubject.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,Boolean.TRUE, null, null)
				,uniformResourceLocatorBusiness.instanciateManyBusinessCrud(Evaluation.class, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE,null, null, null));
		
		//UniformResourceLocator classroomSessionDivisionUpdateStudentResults = new UniformResourceLocator("/private/classroomsessiondivision/updatestudentresults.jsf");
		//UniformResourceLocator classroomSessionDivisionUpdateStudentReport = new UniformResourceLocator("/private/classroomsessiondivision/updatestudentreport.jsf");
		
		instanciateUserAccountsFromActors(inject(TeacherDao.class).readAll(), userRole,teacherRole);
		
		/*inject(UniformResourceLocatorBusiness.class).create(rootDataProducerHelper.getUniformResourceLocators());
		inject(RoleUniformResourceLocatorBusiness.class).create(rootDataProducerHelper.getRoleUniformResourceLocators());
		inject(UserAccountBusiness.class).create(rootDataProducerHelper.getUserAccounts());
		*/
	}
	
	@Override
	public void enableEnterpriseResourcePlanning() {
		StudentBusinessImpl.Listener.COLLECTION.add(new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning(){
    		private static final long serialVersionUID = 1L;
			@Override
			public void beforeCreate(Student student) {
				super.beforeCreate(student);
				if(StringUtils.isBlank(student.getCode())){
					NumberBusiness.FormatArguments orderNumberFormatArguments = new FormatArguments();
					orderNumberFormatArguments.setWidth(4);
					student.setCode("STUD"+Constant.CHARACTER_SLASH+inject(TimeBusiness.class).findYear(inject(AcademicSessionBusiness.class).findCurrent(null).getBirthDate())
							+inject(PersonBusiness.class).findInitials(student.getPerson())+inject(NumberBusiness.class).format(inject(StudentDao.class).countAll()+1,orderNumberFormatArguments)
							+Constant.CHARACTER_HYPHEN+student.getAdmissionLevelTimeDivision().getLevel().getGroup().getCode()
							);
				}
			}
    	});
    	TeacherBusinessImpl.Listener.COLLECTION.add(new TeacherBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning());
	}

	@Override
	protected void setConstants() {}
	
	@Override
	protected void fakeTransactions() {}
	
	/**/
	

	
	/**/
	
    public static SchoolBusinessLayer getInstance() {
		return INSTANCE;
	}
		
}
