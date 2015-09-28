package org.cyk.system.school.business.impl.integration;

import java.math.BigDecimal;

import javax.inject.Inject;
import javax.persistence.EntityManager;

import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.company.business.impl.CompanyBusinessTestHelper;
import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.party.ApplicationBusiness;
import org.cyk.system.root.business.impl.AbstractTestHelper;
import org.cyk.system.root.business.impl.BusinessIntegrationTestHelper;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.business.impl.RootTestHelper;
import org.cyk.system.root.business.impl.validation.AbstractValidator;
import org.cyk.system.root.business.impl.validation.DefaultValidator;
import org.cyk.system.root.business.impl.validation.ExceptionUtils;
import org.cyk.system.root.business.impl.validation.ValidatorMap;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.persistence.impl.GenericDaoImpl;
import org.cyk.system.root.persistence.impl.PersistenceIntegrationTestHelper;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.utility.common.test.DefaultTestEnvironmentAdapter;
import org.cyk.utility.test.ArchiveBuilder;
import org.cyk.utility.test.integration.AbstractIntegrationTestJpaBased;
import org.jboss.shrinkwrap.api.Archive;
import org.junit.Assert;

public abstract class AbstractBusinessIT extends AbstractIntegrationTestJpaBased {

	static {
		AbstractTestHelper.TEST_ENVIRONMENT_LISTENERS.add(new DefaultTestEnvironmentAdapter(){
    		@Override
    		public void assertEquals(String message, Object expected, Object actual) {
    			Assert.assertEquals(message, expected, actual);
    		}
    		@Override
    		public String formatBigDecimal(BigDecimal value) {
    			return RootBusinessLayer.getInstance().getNumberBusiness().format(value);
    		}
    	});
	}
	
	private static final long serialVersionUID = -5752455124275831171L;
	@Inject protected ExceptionUtils exceptionUtils; 
	@Inject protected DefaultValidator defaultValidator;
	@Inject protected GenericDaoImpl g;
	@Inject protected GenericBusiness genericBusiness;
	@Inject protected ApplicationBusiness applicationBusiness;
	@Inject protected TeacherBusiness teacherBusiness;
	
	@Inject protected ValidatorMap validatorMap;// = ValidatorMap.getInstance();
	@Inject protected RootBusinessLayer rootBusinessLayer;
	@Inject protected RootTestHelper rootTestHelper;
	@Inject protected CompanyBusinessLayer companyBusinessLayer;
	@Inject protected CompanyBusinessTestHelper companyBusinessTestHelper;
	@Inject protected SchoolBusinessLayer schoolBusinessLayer;
	@Inject protected SchoolBusinessTestHelper schoolBusinessTestHelper;
	
	/**/
	/*
	protected School highSchool;
	protected File studentClassroomSessionDivisionResultsReportFile;
	protected AcademicSession academicSession;
	protected LevelName levelNameCe2,levelName6eme,levelName2nd,levelNameBachelor;
	protected LevelSpeciality levelSpecialityA,levelSpecialityC,levelSpecialityMaths,levelSpecialityLettre;
	protected Level levelCe2,level6eme,level2ndA,level2ndC,levelBachelorMaths,levelBachelorLettre;
	protected LevelTimeDivision levelTimeDivisionCe21rstYear,levelTimeDivision6eme1rstYear,levelTimeDivision2ndA1rstYear,levelTimeDivision2ndC1rstYear,
	levelTimeDivisionBachelorMaths1rstYear,levelTimeDivisionBachelorMaths2ndYear,levelTimeDivisionBachelorMaths3rdYear,
	levelTimeDivisionBachelorLettre1rstYear,levelTimeDivisionBachelorLettre2ndYear,levelTimeDivisionBachelorLettre3rdYear;
	
	protected ClassroomSession classroomSession;
	protected ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3;
	protected Subject subjectMathsClassroomSessionDivision1,subjectEnglishClassroomSessionDivision1,
		subjectMathsClassroomSessionDivision2,subjectEnglishClassroomSessionDivision2;
	protected EvaluationType interro,devoir;
	protected Collection<Teacher> teachers = new ArrayList<Teacher>();
	*/
	/**/
	
    @Override
    public EntityManager getEntityManager() {
        return g.getEntityManager();
    }
	
    @Override
    protected final void populate() {
    	/*
    	highSchool = new School();
    	highSchool.setCompany(new Company());
    	highSchool.getCompany().setName("CykSchool");
    	highSchool.getCompany().setCreationDate(new Date());
    	highSchool.getCompany().setCode("ANY");
    	highSchool.getCompany().setImage(new File());
    	highSchool.getCompany().getImage().setBytes(RandomDataProvider.getInstance().companyLogo());
    	
    	create(highSchool.getCompany().getImage());
    	
    	highSchool.getCompany().setContactCollection(null);
    	create(highSchool.getCompany());
    	
    	create(highSchool);
    	
    	teachers.add(actor(teacherBusiness,Teacher.class, Boolean.TRUE));
    	teachers.add(actor(teacherBusiness,Teacher.class, Boolean.FALSE));
    	teachers.add(actor(teacherBusiness,Teacher.class, Boolean.TRUE));
    	teachers.add(actor(teacherBusiness,Teacher.class, Boolean.FALSE));
    	
    	studentClassroomSessionDivisionResultsReportFile = new File();
    	try {
    		//studentClassroomSessionDivisionResultsReportFile.setUri(
    		//		new java.io.File("H:\\Repositories\\source code\\git\\org\\cyk\\system\\school\\school-business-impl-ejb\\src\\main\\resources\\META-INF\\report\\student\\result\\"
    		//				+ "test.jrxml").toURI());
    		studentClassroomSessionDivisionResultsReportFile.setBytes(IOUtils.toByteArray(getClass().getResourceAsStream("/META-INF/report/student/result/model1.jrxml")));
		} catch (Exception e) {
			e.printStackTrace();
		}
    	create(studentClassroomSessionDivisionResultsReportFile);
    	TimeDivisionType timeDivisionTypeYear = new TimeDivisionType("YEAR", "YEAR", "YEAR");
    	TimeDivisionType timeDivisionTypeSem = new TimeDivisionType("Semester", "Semester", "Semester");
    	TimeDivisionType timeDivisionTypeTrim = new TimeDivisionType("Trimester", "Trimester", "Trimester");
    	create(timeDivisionTypeYear);create(timeDivisionTypeSem);create(timeDivisionTypeTrim);
    	
    	create(academicSession = new AcademicSession(highSchool,new Period(new Date(), new Date())));
    	levelNameCe2 = levelName("CE2", "CE2",studentClassroomSessionDivisionResultsReportFile,
    			intervalCollection("Mediocre","0","9.99","Passable","10","11.99","Bien","12","15.99","Tres bien","16","20"),
    			intervalCollection("Blame","0","9.99","Encouragement","10","11.99","Felicitations","12","15.99","Honnorable","16","20"),null);
    	levelName6eme = levelName("6eme", "6eme",studentClassroomSessionDivisionResultsReportFile,
    			intervalCollection("Mediocre","0","9.99","Passable","10","11.99","Bien","12","15.99","Tres bien","16","20"),
    			intervalCollection("Blame","0","9.99","Encouragement","10","11.99","Felicitations","12","15.99","Honnorable","16","20"),null);
    	levelName2nd = levelName("2nd", "2nd",studentClassroomSessionDivisionResultsReportFile,
    			intervalCollection("Mediocre","0","9.99","Passable","10","11.99","Bien","12","15.99","Tres bien","16","20"),
    			intervalCollection("Blame","0","9.99","Encouragement","10","11.99","Felicitations","12","15.99","Honnorable","16","20"),null);
    	levelNameBachelor = levelName("Bachelor", "Bachelor",studentClassroomSessionDivisionResultsReportFile,
    			intervalCollection("Mediocre","0","9.99","Passable","10","11.99","Bien","12","15.99","Tres bien","16","20"),
    			intervalCollection("Blame","0","9.99","Encouragement","10","11.99","Felicitations","12","15.99","Honnorable","16","20"),null);
    	create(levelSpecialityA = new LevelSpeciality("A", "A", "A"));
    	create(levelSpecialityC = new LevelSpeciality("C", "C", "C"));
    	create(levelSpecialityMaths = new LevelSpeciality("Maths", "Maths", "Maths"));
    	create(levelSpecialityLettre = new LevelSpeciality("Lettre", "Lettre", "Lettre"));
    	create(levelCe2=new Level(levelNameCe2,null));
    	create(level6eme=new Level(levelName6eme,null));
    	create(level2ndA=new Level(levelName2nd,levelSpecialityA));
    	create(level2ndC=new Level(levelName2nd,levelSpecialityC));
    	create(levelBachelorMaths=new Level(levelNameBachelor,levelSpecialityMaths));
    	create(levelBachelorLettre=new Level(levelNameBachelor,levelSpecialityLettre));
    	create(levelTimeDivisionCe21rstYear=new LevelTimeDivision(levelCe2,timeDivisionTypeYear));
    	create(levelTimeDivision6eme1rstYear=new LevelTimeDivision(level6eme,timeDivisionTypeYear));
    	create(levelTimeDivision2ndA1rstYear=new LevelTimeDivision(level2ndA,timeDivisionTypeYear));
    	create(levelTimeDivision2ndC1rstYear=new LevelTimeDivision(level2ndC,timeDivisionTypeYear));
    	create(levelTimeDivisionBachelorMaths1rstYear=new LevelTimeDivision(levelBachelorMaths,timeDivisionTypeYear));
    	create(levelTimeDivisionBachelorMaths2ndYear=new LevelTimeDivision(levelBachelorMaths,timeDivisionTypeYear));
    	create(levelTimeDivisionBachelorMaths3rdYear=new LevelTimeDivision(levelBachelorMaths,timeDivisionTypeYear));
    	create(levelTimeDivisionBachelorLettre1rstYear=new LevelTimeDivision(levelBachelorLettre,timeDivisionTypeYear));
    	create(levelTimeDivisionBachelorLettre2ndYear=new LevelTimeDivision(levelBachelorLettre,timeDivisionTypeYear));
    	create(levelTimeDivisionBachelorLettre3rdYear=new LevelTimeDivision(levelBachelorLettre,timeDivisionTypeYear));
    	
    	
    	create(classroomSession = new ClassroomSession(academicSession,levelTimeDivisionCe21rstYear,new Period(new Date(), new Date()),
    			(Teacher) RandomDataProvider.getInstance().randomFromList((List<?>) teachers)));
    	
    	create(classroomSessionDivision1 = new ClassroomSessionDivision(classroomSession,timeDivisionTypeTrim,new Period(new Date(), new Date()),new BigDecimal("1")));
    	create(classroomSessionDivision2 = new ClassroomSessionDivision(classroomSession,timeDivisionTypeTrim,new Period(new Date(), new Date()),new BigDecimal("2")));
    	create(classroomSessionDivision3 = new ClassroomSessionDivision(classroomSession,timeDivisionTypeTrim,new Period(new Date(), new Date()),new BigDecimal("2")));
    	
    	interro = evaluationType("Interrogation", "1", "10");
    	devoir = evaluationType("Devoir", "2", "40");
    	
    	SubjectName mathsName = new SubjectName("Maths", "Maths", "Maths");
    	create(mathsName);
    	SubjectName englishName = new SubjectName("Anglais", "Anglais", "Anglais");
    	create(englishName);
    	
    	subjectMathsClassroomSessionDivision1 = subject(classroomSessionDivision1,mathsName, "1");
		subjectEnglishClassroomSessionDivision1 = subject(classroomSessionDivision1,englishName, "1");
		
		subjectMathsClassroomSessionDivision2 = subject(classroomSessionDivision2,mathsName, "4");
		subjectEnglishClassroomSessionDivision2 = subject(classroomSessionDivision2,englishName, "3");
		
    	__populate__();
    	*/
    }
    /*
    protected <T extends AbstractActor> T actor(AbstractActorBusiness<T> business,Class<T> aClass,String name,String lastName,Boolean male){
    	T actor = null;
		try {
			actor = aClass.newInstance();
		} catch (Exception e) {
			e.printStackTrace();
		}
    	actor.setPerson(RootRandomDataProvider.getInstance().person(male));
    	if(name!=null)
    		actor.getPerson().setName(name);
    	if(lastName!=null)
    		actor.getPerson().setLastName(lastName);
    	business.create(actor);
    	return actor;
    }
    
    protected <T extends AbstractActor> T actor(AbstractActorBusiness<T> business,Class<T> aClass,Boolean male){
    	return actor(business, aClass, null, null, male);
    }
    
    protected EvaluationType evaluationType(String name, String coefficient,String maximumValue){
    	EvaluationType evaluationType = new EvaluationType();
    	evaluationType.setName(new EvaluationTypeName(name, name, name));
    	evaluationType.setCoefficient(new BigDecimal(coefficient));
    	evaluationType.setMaximumValue(new BigDecimal(maximumValue));
    	create(evaluationType.getName());
    	create(evaluationType);
    	return evaluationType;
    }
    
    protected Subject subject(ClassroomSessionDivision classroomSessionDivision,SubjectName name, String coefficient){
    	Subject subject = new Subject();
    	subject.setClassroomSessionDivision(classroomSessionDivision);
    	subject.setName(name);
    	subject.setCoefficient(new BigDecimal(coefficient));
    	subject.setTeacher((Teacher) RandomDataProvider.getInstance().randomFromList((List<?>) teachers));
    	create(subject);
    	return subject;
    }
    
    protected IntervalCollection intervalCollection(String...values){
    	IntervalCollection intervalCollection = new IntervalCollection();
    	create(intervalCollection);
    	for(int i=0;i<values.length;i=i+3){
    		Interval interval = new Interval();
    		interval.setCollection(intervalCollection);
    		interval.setName(values[i]);
    		interval.setLow(new BigDecimal(values[i+1]));
    		interval.setHigh(new BigDecimal(values[i+2]));
    		create(interval);
    	}
    	return intervalCollection;
    }
    
    protected LevelName levelName(String code,String name,File studentClassroomSessionDivisionResultsReportFile,IntervalCollection studentSubjectAverageAppreciation
    		,IntervalCollection studentClassroomSessionDivisionAverageAppreciation,IntervalCollection studentClassroomSessionAverageAppreciation){
    	LevelName levelName = new LevelName();
    	levelName.setCode(code);
    	levelName.setName(name);
    	levelName.setStudentClassroomSessionDivisionResultsReportFile(studentClassroomSessionDivisionResultsReportFile);
    	levelName.setStudentSubjectAverageAppreciation(studentSubjectAverageAppreciation);
    	levelName.setStudentClassroomSessionDivisionAverageAppreciation(studentClassroomSessionDivisionAverageAppreciation);
    	levelName.setStudentClassroomSessionAverageAppreciation(studentClassroomSessionAverageAppreciation);
    	levelName.setStudentClassroomSessionDivisionResultsReportHeadRight(
    			"Republique de COTE D'IVOIRE\r\n\r\n"
				+"Union - Discipline - Travail\r\n\r\n"
				+"Ministere de l'education nationale");
    	create(levelName);
    	return levelName;
    }
    */
    
    //protected abstract void __populate__();

    protected void installApplication(Boolean fake){
    	schoolBusinessLayer.installApplication(fake);
    }
    
    protected void installApplication(){
    	installApplication(Boolean.TRUE);
    }
    
	@Override
    protected void _execute_() {
        super._execute_();
        create();    
        read(); 
        update();    
        delete();    
        finds();
        businesses();
    }
    
	protected void finds(){}
	
	protected void businesses(){}
	
	/* Shortcut */
    
    protected AbstractIdentifiable create(AbstractIdentifiable object){
        return genericBusiness.create(object);
    }
    
    protected AbstractIdentifiable update(AbstractIdentifiable object){
        return genericBusiness.update(object);
    }
    
    protected void validate(Object object){
        if(object==null)
            return;
        @SuppressWarnings("unchecked")
        AbstractValidator<Object> validator = (AbstractValidator<Object>) validatorMap.validatorOf(object.getClass());
        if(validator==null){
            //log.warning("No validator has been found. The default one will be used");
            //validator = defaultValidator;
            return;
        }
        try {
            validator.validate(object);
        } catch (Exception e) {}
        
        if(!Boolean.TRUE.equals(validator.isSuccess()))
            System.out.println(validator.getMessagesAsString());
        
    }
    
    public static Archive<?> createRootDeployment() {
        return  
                new ArchiveBuilder().create().getArchive().
                    addClasses(BusinessIntegrationTestHelper.classes()).
                    addClasses(PersistenceIntegrationTestHelper.classes()).
                    addClasses(RootBusinessLayer.class,RootTestHelper.class,CompanyBusinessLayer.class).
                    addPackages(Boolean.FALSE, BusinessIntegrationTestHelper.packages()).
                    addPackages(Boolean.TRUE,"org.cyk.system.company").
                    addPackages(Boolean.TRUE,"org.cyk.system.school") 
                ;
    } 
    
    @Override
    protected void create() {
        
    }

    @Override
    protected void delete() {
        
    }

    

    @Override
    protected void read() {
        
    }

    @Override
    protected void update() {
        
    }
}
