package org.cyk.system.school.business.impl.integration;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.business.impl.RootBusinessLayer;
import org.cyk.system.root.model.file.File;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.SchoolBusinessImpl;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.EvaluationTypeName;
import org.cyk.system.school.model.subject.SubjectName;

@Singleton
public class IesaFakedDataProducer extends AbstractFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	
	private SubjectName subjectNameEnglishLanguage,subjectNameLiteratureInEnglish,subjectNameHistory,subjectNameGeography
		,subjectNameSocialStudies,subjectNameReligiousStudies,subjectNameMathematics,subjectNamePhysics,subjectNameChemistry,subjectNameBiology,subjectNameFrench
		,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation,subjectNameGrammar,subjectNameReadingComprehension,subjectNameHandWriting,
		subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameScience;
	private EvaluationTypeName evaluationTypeNameTest1,evaluationTypeNameTest2,evaluationTypeNameExam;
	private Interval intervalGradingScaleAStar,intervalGradingScaleA,intervalGradingScaleB,intervalGradingScaleC,intervalGradingScaleD,intervalGradingScaleE;
	private LevelName levelNameG1,levelNameG2,levelNameG3;
	private Level levelG1,levelG2,levelG3;
	private LevelTimeDivision levelTimeDivisionG1,levelTimeDivisionG2,levelTimeDivisionG3;
	private ClassroomSession classroomSessionG1,classroomSessionG2,classroomSessionG3;
	private ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3; 
	private EvaluationType evaluationTypeTest1,evaluationTypeTest2,evaluationTypeTest3;
	
	@Override
	public void produce() {
		rootDataProducerHelper.setBasePackage(SchoolBusinessLayer.class.getPackage());
		// Subjects
		subjectNameEnglishLanguage = createEnumeration(SubjectName.class,"English Language");
		subjectNameLiteratureInEnglish = createEnumeration(SubjectName.class,"Literature in english");
		subjectNameHistory = createEnumeration(SubjectName.class,"History");
		subjectNameGeography = createEnumeration(SubjectName.class,"Geography");
		subjectNameSocialStudies = createEnumeration(SubjectName.class,"Social Studies");
		subjectNameReligiousStudies = createEnumeration(SubjectName.class,"Religious studies/Divinity");
		subjectNameMathematics = createEnumeration(SubjectName.class,"Mathematics");
		subjectNamePhysics = createEnumeration(SubjectName.class,"Physics");
		subjectNameChemistry = createEnumeration(SubjectName.class,"Chemistry");
		subjectNameBiology = createEnumeration(SubjectName.class,"Biology");
		subjectNameFrench = createEnumeration(SubjectName.class,"French");
		subjectNameArtAndCraft = createEnumeration(SubjectName.class,"Art & Craft");
		subjectNameMusic = createEnumeration(SubjectName.class,"Music");
		subjectNameICT = createEnumeration(SubjectName.class,"ICT");
		subjectNamePhysicalEducation = createEnumeration(SubjectName.class,"Physical education");
		
		//Evaluation Type
		evaluationTypeNameTest1 = createEnumeration(EvaluationTypeName.class,"Test 1");
		evaluationTypeNameTest2 = createEnumeration(EvaluationTypeName.class,"Test 2");
		evaluationTypeNameExam = createEnumeration(EvaluationTypeName.class,"Exam");
		
		//Grades
		IntervalCollection intervalCollection = new IntervalCollection();
		intervalGradingScaleAStar = createInterval(intervalCollection,"A*", "Outstanding", "90", "100");
		intervalGradingScaleA = createInterval(intervalCollection,"A", "Excellent", "80", "89");
		intervalGradingScaleB = createInterval(intervalCollection,"B", "Very Good", "70", "79");
		intervalGradingScaleC = createInterval(intervalCollection,"C", "Good", "60", "69");
		intervalGradingScaleD = createInterval(intervalCollection,"D", "Satisfactory", "50", "59");
		intervalGradingScaleE = createInterval(intervalCollection,"E", "Fail", "0", "49"); 
		CommonNodeInformations commonNodeInformations = new CommonNodeInformations(intervalCollection,createFile("report/model1.jrxml", "model1.jrxml"));
		
		//Level names
		levelNameG1 = createEnumeration(LevelName.class,"Grade 1");
		levelNameG2 = createEnumeration(LevelName.class,"Grade 2");
		levelNameG3 = createEnumeration(LevelName.class,"Grade 3");
		
		levelG1 = create(new Level(levelNameG1, null));
		levelG2 = create(new Level(levelNameG2, null));
		levelG3 = create(new Level(levelNameG3, null));
		
		levelTimeDivisionG1 = create(new LevelTimeDivision(levelG1, getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR)));
		levelTimeDivisionG2 = create(new LevelTimeDivision(levelG2, getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR)));
		levelTimeDivisionG3 = create(new LevelTimeDivision(levelG3, getEnumeration(TimeDivisionType.class,TimeDivisionType.YEAR)));
		
		/**/
		
		School school = new School(ownedCompanyBusiness.findDefaultOwnedCompany(),commonNodeInformations);
    	create(school);
    	
    	AcademicSession academicSession; 
    	create(academicSession = new AcademicSession(school,new Period(new Date(), new Date())));
    	
    	classroomSessionG1 = create(new ClassroomSession(academicSession, levelTimeDivisionG1, new Period(new Date(), new Date()), null));
    	classroomSessionG2 = create(new ClassroomSession(academicSession, levelTimeDivisionG2, new Period(new Date(), new Date()), null));
    	classroomSessionG3 = create(new ClassroomSession(academicSession, levelTimeDivisionG3, new Period(new Date(), new Date()), null));
    	
    	classroomSessionDivision1 = create(new ClassroomSessionDivision(classroomSessionG1,getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER)
    			,new Period(new Date(), new Date()),new BigDecimal("1")));
    	classroomSessionDivision2 = create(new ClassroomSessionDivision(classroomSessionG1,getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER)
    			,new Period(new Date(), new Date()),new BigDecimal("1")));
    	classroomSessionDivision3 = create(new ClassroomSessionDivision(classroomSessionG1,getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER)
    			,new Period(new Date(), new Date()),new BigDecimal("1")));
    	
    	evaluationTypeTest1 = create(new EvaluationType());
    	
    	/*
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
		*/
	}

}
