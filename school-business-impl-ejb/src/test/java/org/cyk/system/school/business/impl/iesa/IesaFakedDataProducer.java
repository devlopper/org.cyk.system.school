package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.api.structure.CompanyBusiness;
import org.cyk.system.company.business.api.structure.OwnedCompanyBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.model.event.Event;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.IntervalCollection;
import org.cyk.system.root.model.mathematics.MetricCollection;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.LectureBusiness;
import org.cyk.system.school.business.api.subject.StudentSubjectBusiness;
import org.cyk.system.school.business.api.subject.SubjectEvaluationBusiness;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionDivisionInfos;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionDivisionSubjectInfos;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionInfos;
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
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.EvaluationType;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.system.school.persistence.api.subject.StudentSubjectDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.generator.RandomDataProvider;

import lombok.Getter;
import lombok.Setter;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;

	private SchoolBusinessLayer schoolBusinessLayer = SchoolBusinessLayer.getInstance();
	@Inject private OwnedCompanyBusiness ownedCompanyBusiness;
	@Inject private CompanyBusiness companyBusiness;
	@Inject private SubjectEvaluationBusiness subjectEvaluationBusiness;
	@Inject private StudentSubjectDao studentSubjectDao;
	@Inject private StudentSubjectBusiness studentSubjectBusiness;
	@Inject private StudentBusiness studentBusiness;
	@Inject private ClassroomSessionDivisionSubjectBusiness classroomSessionDivisionSubjectBusiness;
	@Inject private LectureBusiness lectureBusiness;
	
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
	private Collection<SubjectEvaluationType> subjectEvaluationTypes = new ArrayList<>();
	
	private MetricCollection studentWorkMetricCollection;
	
	private CommonNodeInformations commonNodeInformations;
	
	@Setter private Integer numbreOfTeachers = 10;
	@Setter private Integer numbreOfStudents = 10;
	@Setter private Integer numbreOfLecturesByClassroomSessionDivisionSubject = 5;
	@Setter private Integer numbreOfStudentsByClassroomSession = 25;
	
	@Setter private Boolean generateCompleteAcademicSession = Boolean.FALSE;
	
	@Override
	public void produce(FakedDataProducerListener listener) {
		rootDataProducerHelper.setBasePackage(SchoolBusinessLayer.class.getPackage());
		//schoolBusinessLayer.setAverageComputationListener(new Averagec);
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
		studentWorkMetricCollection.getValueIntervalCollection().addItem("1", "Has no regard for the observable traits", "1", "1");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("2", "Shows minimal regard for the observable traits", "2", "2");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("3", "Acceptable level of observable traits", "3", "3");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("4", "Maintains high level of observable traits", "4", "4");
		studentWorkMetricCollection.getValueIntervalCollection().addItem("5", "Maintains an excellent degree of observable traits", "5", "5");
		
		create(studentWorkMetricCollection);
		commonNodeInformations = new CommonNodeInformations(intervalCollection,studentWorkMetricCollection,createFile("report/iesa.jrxml", "reportcard.jrxml"));
		
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
		flush(listener,"Before actors");
		
		rootRandomDataProvider.createActor(Teacher.class, numbreOfTeachers);
		flush(listener,"Teachers");
		rootRandomDataProvider.createActor(Student.class, numbreOfStudents);
		flush(listener,"Students");
		
		School school = new School(ownedCompanyBusiness.findDefaultOwnedCompany(),commonNodeInformations);
    	create(school);
    	
    	school.getOwnedCompany().getCompany().setManager(rootRandomDataProvider.oneFromDatabase(Person.class));
    	companyBusiness.update(school.getOwnedCompany().getCompany());
    	
    	AcademicSession academicSession = new AcademicSession(school,commonNodeInformations);
    	academicSession.getPeriod().setFromDate(new Date());
    	academicSession.getPeriod().setToDate(new Date());
    	academicSession = create(academicSession);
    	flush(listener,"Academic session");
    	
    	grade1 = grade(academicSession, levelTimeDivisionG1,new Subject[]{subjectNameMathematics,subjectNameGrammar,subjectNameReadingComprehension
    			,subjectNameHandWriting,subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameSocialStudies
    			,subjectNameScience,subjectNameFrench,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation},listener);
    	
    	grade2 = grade(academicSession, levelTimeDivisionG2,new Subject[]{subjectNameMathematics,subjectNameGrammar,subjectNameReadingComprehension
    			,subjectNameHandWriting,subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameSocialStudies
    			,subjectNameScience,subjectNameFrench,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation},listener);
    	
    	grade3 = grade(academicSession, levelTimeDivisionG3,new Subject[]{subjectNameMathematics,subjectNameGrammar,subjectNameReadingComprehension
    			,subjectNameHandWriting,subjectNameSpelling,subjectNamePhonics,subjectNameCreativeWriting,subjectNameMoralEducation,subjectNameSocialStudies
    			,subjectNameScience,subjectNameFrench,subjectNameArtAndCraft,subjectNameMusic,subjectNameICT,subjectNamePhysicalEducation},listener);
    	
    	if(Boolean.TRUE.equals(generateCompleteAcademicSession)){
    		doBusiness(listener);
    	}
	}
	
	private ClassroomSessionInfos grade(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Subject[] subjects,FakedDataProducerListener listener){
		ClassroomSessionInfos classroomSessionInfos = new ClassroomSessionInfos(createClassroomSession(academicSession, levelTimeDivision));
		classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionInfos.getClassroomSession(),subjects));
		classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionInfos.getClassroomSession(),subjects));
		classroomSessionInfos.getDivisions().add(createClassroomSessionDivision(classroomSessionInfos.getClassroomSession(),subjects));
		flush(listener,"Grade");
		return classroomSessionInfos;
	}
	
	private void doBusiness(FakedDataProducerListener listener){
		for(ClassroomSessionInfos classroomSessionInfos : new ClassroomSessionInfos[]{grade1,grade2,grade3}){
			for(Student student : studentBusiness.findManyRandomly(numbreOfStudentsByClassroomSession)){
				for(ClassroomSessionDivisionInfos classroomSessionDivisionInfos : classroomSessionInfos.getDivisions()){
					for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionInfos.getClassroomSessionDivisionSubjects()){
						StudentSubject studentSubject = new StudentSubject(student, classroomSessionDivisionSubject);
						studentSubjectBusiness.create(studentSubject);
					}
				}
				
			}
			flush(listener,"Student subjects "+classroomSessionInfos.getClassroomSession());
		}
		
		
		for(SubjectEvaluationType subjectEvaluationType : subjectEvaluationTypes){
			SubjectEvaluation subjectEvaluation = new SubjectEvaluation(subjectEvaluationType, Boolean.FALSE);
			for(StudentSubject studentSubject :studentSubjectDao.readBySubject(subjectEvaluationType.getSubject()) ){
				StudentSubjectEvaluation studentSubjectEvaluation = new StudentSubjectEvaluation(subjectEvaluation, studentSubject
						, new BigDecimal(RandomDataProvider.getInstance().randomInt(0, subjectEvaluationType.getMaximumValue().intValue())));
				subjectEvaluation.getStudentSubjectEvaluations().add(studentSubjectEvaluation);
			}
			subjectEvaluationBusiness.create(subjectEvaluation);
		}
		flush(listener,"Student subject evaluations");
		
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : classroomSessionDivisionSubjectBusiness.findAll()){
			for(int i=0;i<numbreOfLecturesByClassroomSessionDivisionSubject;i++){
				Event event = new Event();
				event.getPeriod().setFromDate(new Date());
				event.getPeriod().setToDate(new Date());
				Lecture lecture = new Lecture(classroomSessionDivisionSubject, event);
				lectureBusiness.create(lecture);
			}
		}
		flush(listener,"Lectures");
	}
	
	/**/
	
	private LevelName createLevelName(String name){
		LevelName levelName = new LevelName();
		levelName.setCode(StringUtils.replace(name, Constant.CHARACTER_SPACE.toString(), Constant.EMPTY_STRING));
		levelName.setName(name);
		levelName.setNodeInformations(commonNodeInformations);
		return create(levelName);
	}
	
	private ClassroomSession createClassroomSession(AcademicSession academicSession,LevelTimeDivision levelTimeDivision){
		ClassroomSession classroomSession = new ClassroomSession(academicSession, levelTimeDivision, rootRandomDataProvider.oneFromDatabase(Teacher.class));
		classroomSession.getPeriod().setFromDate(new Date());
		classroomSession.getPeriod().setToDate(new Date());
		return create(classroomSession);
	}
	
	private ClassroomSessionDivisionInfos createClassroomSessionDivision(ClassroomSession classroomSession,Subject[] subjects){
		ClassroomSessionDivision classroomSessionDivision = new ClassroomSessionDivision(classroomSession,getEnumeration(TimeDivisionType.class,TimeDivisionType.TRIMESTER)
    			,new BigDecimal("1"));
		classroomSessionDivision.getPeriod().setFromDate(new Date());
		classroomSessionDivision.getPeriod().setToDate(new Date());
		ClassroomSessionDivisionInfos classroomSessionDivisionInfos = new ClassroomSessionDivisionInfos(create(classroomSessionDivision));
		
		for(Subject subject : subjects){
			classroomSessionDivisionInfos.getSubjects().add(createClassroomSessionDivisionSubject(classroomSessionDivision,subject,new Object[][]{
				{evaluationTypeTest1,"0.15"},{evaluationTypeTest2,"0.15"},{evaluationTypeExam,"0.7"}
			}));
    	}
    	
		return classroomSessionDivisionInfos;
	}
	
	private ClassroomSessionDivisionSubjectInfos createClassroomSessionDivisionSubject(ClassroomSessionDivision classroomSessionDivision,Subject subject,Object[][] evaluationTypes){
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = new ClassroomSessionDivisionSubject(classroomSessionDivision,subject,BigDecimal.ONE,rootRandomDataProvider.oneFromDatabase(Teacher.class));
		classroomSessionDivisionSubject = create(classroomSessionDivisionSubject);
		ClassroomSessionDivisionSubjectInfos classroomSessionDivisionSubjectInfos = new ClassroomSessionDivisionSubjectInfos(classroomSessionDivisionSubject);
		for(Object[] evaluationType : evaluationTypes){
			Object[] infos = evaluationType;
			classroomSessionDivisionSubjectInfos.getEvaluationTypes().add(createSubjectEvaluationType(classroomSessionDivisionSubject, (EvaluationType)infos[0], new BigDecimal((String)infos[1])));
		}
		return classroomSessionDivisionSubjectInfos;
	}
	
	private SubjectEvaluationType createSubjectEvaluationType(ClassroomSessionDivisionSubject subject,EvaluationType name,BigDecimal coefficient){
		SubjectEvaluationType subjectEvaluationType = create(new SubjectEvaluationType(subject,name,coefficient,new BigDecimal("100")));
		subjectEvaluationTypes.add(subjectEvaluationType);
		return subjectEvaluationType;
	}
	
	/**/
		
	public static class ReportProducer extends AbstractSchoolReportProducer{
		private static final long serialVersionUID = 246685915578107971L;
    	
		@Override
		public StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision) {
			StudentClassroomSessionDivisionReport r = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision);
			r.getSubjectsTableColumnNames().add("No.");
			r.getSubjectsTableColumnNames().add("SUBJECTS");
			r.getSubjectsTableColumnNames().add("Test 1 15%");
			r.getSubjectsTableColumnNames().add("Test 2 15%");
			r.getSubjectsTableColumnNames().add("Exam 70%");
			r.getSubjectsTableColumnNames().add("TOTAL");
			r.getSubjectsTableColumnNames().add("GRADE");
			r.getSubjectsTableColumnNames().add("RANK");
			r.getSubjectsTableColumnNames().add("OUT OF");
			r.getSubjectsTableColumnNames().add("MAX");
			r.getSubjectsTableColumnNames().add("CLASS AVERAGE");
			r.getSubjectsTableColumnNames().add("REMARKS");
			r.getSubjectsTableColumnNames().add("TEACHER");
			
			sumMarks(r, 3);
			
			r.setInformationLabelValueCollection(labelValueCollection("school.report.studentclassroomsessiondivision.block.informations"));
			labelValue("school.report.studentclassroomsessiondivision.block.informations.annualaverage", "To Compute");
			labelValue("school.report.studentclassroomsessiondivision.block.informations.annualgrade", "To Compute");
			labelValue("school.report.studentclassroomsessiondivision.block.informations.annualrank", "To Compute");
			labelValue("school.report.studentclassroomsessiondivision.block.informations.promotion", "To Compute");
			labelValue("school.report.studentclassroomsessiondivision.block.informations.nextacademicsession", "To Compute");
			
			r.setBehaviorLabelValueCollection1(new LabelValueCollectionReport());
			r.getBehaviorLabelValueCollection1().setName("BEHAVIOUR,STUDY AND WORK HABITS");
			for(int i=0;i<=5;i++)
				r.getBehaviorLabelValueCollection1().getCollection().add(r.getBehaviorLabelValueCollection().getCollection().get(i));
			
			r.setBehaviorLabelValueCollection2(new LabelValueCollectionReport());
			r.getBehaviorLabelValueCollection2().setName("BEHAVIOUR,STUDY AND WORK HABITS");
			for(int i=6;i<=11;i++)
				r.getBehaviorLabelValueCollection2().getCollection().add(r.getBehaviorLabelValueCollection().getCollection().get(i));
			
			return r;
		}
    }
	
	

}
