package org.cyk.system.school.business.impl.integration;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.inject.Inject;
import javax.inject.Singleton;

import lombok.Getter;

import org.apache.commons.lang3.StringUtils;
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
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.CommonNodeInformations;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelName;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.system.school.model.session.School;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.utility.common.Constant;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	@Inject private SchoolBusinessLayer schoolBusinessLayer;
	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	
	private Subject subjectNameEnglishLanguage,subjectNameLiteratureInEnglish,subjectNameHistory,subjectNameGeography
		,subjectNameSocialStudies,subjectNameReligiousStudies,subjectNameMathematics,subjectNamePhysics,subjectNameChemistry,subjectNameBiology,subjectNameFrench
		,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation,subjectNameGrammar,subjectNameReadingComprehension,subjectNameHandWriting,
		subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameScience;
	private EvaluationType evaluationTypeNameTest1,evaluationTypeNameTest2,evaluationTypeNameExam;
	private Interval intervalGradingScaleAStar,intervalGradingScaleA,intervalGradingScaleB,intervalGradingScaleC,intervalGradingScaleD,intervalGradingScaleE;
	private LevelName levelNameG1,levelNameG2,levelNameG3;
	private Level levelG1,levelG2,levelG3;
	private LevelTimeDivision levelTimeDivisionG1,levelTimeDivisionG2,levelTimeDivisionG3;
	private ClassroomSession classroomSessionG1,classroomSessionG2,classroomSessionG3;
	private ClassroomSessionDivision classroomSessionDivision1,classroomSessionDivision2,classroomSessionDivision3; 
	private ClassroomSessionDivisionSubject subjectEnglishLanguage,subjectLiteratureInEnglish,subjectHistory,subjectGeography
	,subjectSocialStudies,subjectReligiousStudies,subjectMathematics,subjectPhysics,subjectChemistry,subjectBiology,subjectFrench
	,subjectArtAndCraft,subjectMusic,subjectICT,subjectPhysicalEducation,subjectGrammar,subjectReadingComprehension,subjectHandWriting,
	subjectSpelling,subjectPhonics,subjectCreativeWriting,subjectMoralEducation,subjectScience;
	
	private CommonNodeInformations commonNodeInformations;
	
	@Override
	public void produce() {
		rootDataProducerHelper.setBasePackage(SchoolBusinessLayer.class.getPackage());
		//schoolBusinessLayer.setAverageComputationListener(new Averagec);
		// Subjects
		subjectNameEnglishLanguage = createEnumeration(Subject.class,"English Language");
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
		evaluationTypeNameTest1 = createEnumeration(EvaluationType.class,"Test 1");
		evaluationTypeNameTest2 = createEnumeration(EvaluationType.class,"Test 2");
		evaluationTypeNameExam = createEnumeration(EvaluationType.class,"Exam");
		
		//Grades
		IntervalCollection intervalCollection = new IntervalCollection();
		intervalGradingScaleAStar = createInterval(intervalCollection,"A*", "Outstanding", "90", "100");
		intervalGradingScaleA = createInterval(intervalCollection,"A", "Excellent", "80", "89");
		intervalGradingScaleB = createInterval(intervalCollection,"B", "Very Good", "70", "79");
		intervalGradingScaleC = createInterval(intervalCollection,"C", "Good", "60", "69");
		intervalGradingScaleD = createInterval(intervalCollection,"D", "Satisfactory", "50", "59");
		intervalGradingScaleE = createInterval(intervalCollection,"E", "Fail", "0", "49"); 
		commonNodeInformations = new CommonNodeInformations(intervalCollection,createFile("report/model1.jrxml", "model1.jrxml"));
		
		//Level names
		levelNameG1 = createLevelName("Grade 1");
		levelNameG2 = createLevelName("Grade 2");
		levelNameG3 = createLevelName("Grade 3");
		
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
    	
    	subjectEnglishLanguage = createSubject(classroomSessionDivision1,subjectNameEnglishLanguage,null);
    	subjectFrench = createSubject(classroomSessionDivision1,subjectNameFrench,null);
    	subjectBiology = createSubject(classroomSessionDivision1,subjectNameBiology,null);
    	subjectChemistry = createSubject(classroomSessionDivision1,subjectNameChemistry,null);
    	subjectGeography = createSubject(classroomSessionDivision1,subjectNameGeography,null);
    	
    	for(ClassroomSessionDivisionSubject subject : new ClassroomSessionDivisionSubject[]{subjectEnglishLanguage,subjectFrench,subjectBiology,subjectChemistry,subjectGeography}){
    		createEvaluationType(subject, evaluationTypeNameTest1,new BigDecimal(".15"));
    		createEvaluationType(subject, evaluationTypeNameTest2,new BigDecimal(".15"));
    		createEvaluationType(subject, evaluationTypeNameExam,new BigDecimal(".7"));
    	}
    	
	}
	
	private LevelName createLevelName(String name){
		LevelName levelName = new LevelName();
		levelName.setCode(StringUtils.replace(name, Constant.CHARACTER_SPACE.toString(), Constant.EMPTY_STRING));
		levelName.setName(name);
		levelName.setNodeInformations(commonNodeInformations);
		return create(levelName);
	}
	
	private ClassroomSessionDivisionSubject createSubject(ClassroomSessionDivision classroomSessionDivision,Subject subjectName,Teacher teacher){
		return create(new ClassroomSessionDivisionSubject(classroomSessionDivision,subjectName,BigDecimal.ONE,teacher));
	}
	
	private SubjectEvaluationType createEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType name,BigDecimal coefficient){
		return create(new SubjectEvaluationType(subject,name,coefficient,new BigDecimal("100")));
	}
	
	/**/

}
