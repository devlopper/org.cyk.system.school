package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;

import org.cyk.system.company.business.api.structure.CompanyBusiness;
import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.root.business.api.TypedBusiness;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.mathematics.MathematicsBusiness.AverageComputationListener;
import org.cyk.system.root.business.impl.AbstractBusinessLayer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.file.Script;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.EvaluationTypeName;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.model.subject.SubjectName;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER)
public class SchoolBusinessLayer extends AbstractBusinessLayer implements Serializable {

	private static final long serialVersionUID = -7434478805525552120L;
	public static final int DEPLOYMENT_ORDER = CompanyBusinessLayer.DEPLOYMENT_ORDER+1;
	
	//@Inject private TeacherBusiness teacherBusiness;
	@Inject private FileBusiness fileBusiness;
	@Inject private CompanyBusiness companyBusiness;
	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	@Inject private StudentBusiness studentBusiness;
	
	@Getter private AverageComputationListener averageComputationListener;
	@Getter private Script averageComputationScript;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		registerResourceBundle("org.cyk.system.school.model.resources.entity",getClass().getClassLoader());
		registerResourceBundle("org.cyk.system.school.business.message",getClass().getClassLoader());
		registerResourceBundle("org.cyk.system.school.business.ui",getClass().getClassLoader());
	}

	@Override
	protected void persistData() {
		
	}

	@Override
	protected void setConstants() {
		
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public void registerTypedBusinessBean(Map<Class<AbstractIdentifiable>, TypedBusiness<AbstractIdentifiable>> beansMap) {
        beansMap.put((Class)Student.class, (TypedBusiness)studentBusiness);
        
        
        
    }

	@Override
	protected void fakeTransactions() {
		/*
		School highSchool;
		File studentClassroomSessionDivisionResultsReportFile;
		AcademicSession academicSession;
		LevelName levelNameCe2,levelName6eme,levelName2nd,levelNameBachelor;
		LevelSpeciality levelSpecialityA,levelSpecialityC,levelSpecialityMaths,levelSpecialityLettre;
		Level levelCe2,level6eme,level2ndA,level2ndC,levelBachelorMaths,levelBachelorLettre;
		LevelTimeDivision levelTimeDivisionCe21rstYear,levelTimeDivision6eme1rstYear,levelTimeDivision2ndA1rstYear,levelTimeDivision2ndC1rstYear,
		levelTimeDivisionBachelorMaths1rstYear,levelTimeDivisionBachelorMaths2ndYear,levelTimeDivisionBachelorMaths3rdYear,
		levelTimeDivisionBachelorLettre1rstYear,levelTimeDivisionBachelorLettre2ndYear,levelTimeDivisionBachelorLettre3rdYear;
		
		ClassroomSession classroomSession;
		ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3;
		Subject subjectMathsClassroomSessionDivision1,subjectEnglishClassroomSessionDivision1,
			subjectMathsClassroomSessionDivision2,subjectEnglishClassroomSessionDivision2;
		EvaluationType interro,devoir;
		//List<Teacher> teachers = new ArrayList<>();
		
		Company company = new Company();
		company.setName("CykSchool");
    	company.setCreationDate(new Date());
    	company.setCode("ANY");
    	company.setImage(new File());
    	company.getImage().setBytes(RandomDataProvider.getInstance().companyLogo());
    	company.getImage().setExtension("jpeg");
    	company.setContactCollection(null);
    	create(company.getImage());
    	companyBusiness.create(company);
    	
    	highSchool = new School();
    	highSchool.setOwnedCompany(new OwnedCompany());
    	highSchool.getOwnedCompany().setCompany(company);
    	ownedCompanyBusiness.create(highSchool.getOwnedCompany());
    	
    	create(highSchool);
    	
    	RootRandomDataProvider.getInstance().createActor(Teacher.class, 4);
    	
    	studentClassroomSessionDivisionResultsReportFile = null;//new File();
    	try {
    		//studentClassroomSessionDivisionResultsReportFile.setUri(
    		//		new java.io.File("H:\\Repositories\\source code\\git\\org\\cyk\\system\\school\\school-business-impl-ejb\\src\\main\\resources\\META-INF\\report\\student\\result\\"
    		//				+ "test.jrxml").toURI());
    		studentClassroomSessionDivisionResultsReportFile = 
    				fileBusiness.process(IOUtils.toByteArray(getClass().getResourceAsStream("/META-INF/report/student/result/model1.jrxml")), "model1.jrxml");
    		//studentClassroomSessionDivisionResultsReportFile.setBytes(IOUtils.toByteArray(getClass().getResourceAsStream("/META-INF/report/student/result/model1.jrxml")));
		} catch (Exception e) {
			e.printStackTrace();
		}
    	//debug(studentClassroomSessionDivisionResultsReportFile);
    	studentClassroomSessionDivisionResultsReportFile.setExtension("png");
    	create(studentClassroomSessionDivisionResultsReportFile);
    	
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
    	create(levelTimeDivisionCe21rstYear=new LevelTimeDivision(levelCe2,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivision6eme1rstYear=new LevelTimeDivision(level6eme,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivision2ndA1rstYear=new LevelTimeDivision(level2ndA,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivision2ndC1rstYear=new LevelTimeDivision(level2ndC,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivisionBachelorMaths1rstYear=new LevelTimeDivision(levelBachelorMaths,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivisionBachelorMaths2ndYear=new LevelTimeDivision(levelBachelorMaths,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivisionBachelorMaths3rdYear=new LevelTimeDivision(levelBachelorMaths,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivisionBachelorLettre1rstYear=new LevelTimeDivision(levelBachelorLettre,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivisionBachelorLettre2ndYear=new LevelTimeDivision(levelBachelorLettre,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	create(levelTimeDivisionBachelorLettre3rdYear=new LevelTimeDivision(levelBachelorLettre,RootBusinessLayer.getInstance().getTimeDivisionTypeYear()));
    	
    	
    	create(classroomSession = new ClassroomSession(academicSession,levelTimeDivisionCe21rstYear,new Period(new Date(), new Date()),
    			RootRandomDataProvider.getInstance().actor(Teacher.class)));
    	
    	create(classroomSessionDivision1 = new ClassroomSessionDivision(classroomSession,RootBusinessLayer.getInstance().getTimeDivisionTypeTrimester(),new Period(new Date(), new Date()),new BigDecimal("1")));
    	create(classroomSessionDivision2 = new ClassroomSessionDivision(classroomSession,RootBusinessLayer.getInstance().getTimeDivisionTypeTrimester(),new Period(new Date(), new Date()),new BigDecimal("2")));
    	create(classroomSessionDivision3 = new ClassroomSessionDivision(classroomSession,RootBusinessLayer.getInstance().getTimeDivisionTypeTrimester(),new Period(new Date(), new Date()),new BigDecimal("2")));
    	
    	interro = evaluationType("Interrogation", "1", "10");
    	devoir = evaluationType("Devoir", "2", "40");
    	
    	SubjectName mathsName = new SubjectName("Maths", "Maths", "Maths");
    	create(mathsName);
    	SubjectName englishName = new SubjectName("Anglais", "Anglais", "Anglais");
    	create(englishName);
    	
    	subjectMathsClassroomSessionDivision1 = subject(classroomSessionDivision1,mathsName, "1",RootRandomDataProvider.getInstance().actor(Teacher.class));
		subjectEnglishClassroomSessionDivision1 = subject(classroomSessionDivision1,englishName, "1",RootRandomDataProvider.getInstance().actor(Teacher.class));
		
		subjectMathsClassroomSessionDivision2 = subject(classroomSessionDivision2,mathsName, "4",RootRandomDataProvider.getInstance().actor(Teacher.class));
		subjectEnglishClassroomSessionDivision2 = subject(classroomSessionDivision2,englishName, "3",RootRandomDataProvider.getInstance().actor(Teacher.class));
		*/
		
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
    
    protected Subject subject(ClassroomSessionDivision classroomSessionDivision,SubjectName name, String coefficient,Teacher teacher){
    	Subject subject = new Subject();
    	subject.setClassroomSessionDivision(classroomSessionDivision);
    	subject.setName(name);
    	subject.setCoefficient(new BigDecimal(coefficient));
    	subject.setTeacher(teacher);
    	create(subject);
    	return subject;
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
	
}
