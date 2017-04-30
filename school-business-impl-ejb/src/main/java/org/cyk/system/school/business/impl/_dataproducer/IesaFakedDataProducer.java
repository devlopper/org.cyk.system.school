package org.cyk.system.school.business.impl._dataproducer;

import java.io.File;
import java.io.FileInputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;

import javax.inject.Singleton;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.system.company.business.api.product.IntangibleProductBusiness;
import org.cyk.system.company.business.api.product.TangibleProductBusiness;
import org.cyk.system.company.business.api.sale.SalableProductBusiness;
import org.cyk.system.root.business.api.file.FileBusiness;
import org.cyk.system.root.business.api.geography.ContactCollectionBusiness;
import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.business.api.geography.LocationBusiness;
import org.cyk.system.root.business.api.party.person.PersonBusiness;
import org.cyk.system.root.business.api.party.person.PersonRelationshipBusiness;
import org.cyk.system.root.business.api.security.UserAccountBusiness;
import org.cyk.system.root.business.impl.IdentifiableExcelSheetReader;
import org.cyk.system.root.business.impl.IdentifiableInstanceFieldSetterAdapter.OneDimensionObjectArray;
import org.cyk.system.root.business.impl.IdentifiableInstanceFieldSetterAdapter.TwoDimensionObjectArray;
import org.cyk.system.root.business.impl.OneDimensionObjectArrayAdapter;
import org.cyk.system.root.business.impl.language.LanguageBusinessImpl;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.model.geography.ContactCollection;
import org.cyk.system.root.model.geography.ElectronicMail;
import org.cyk.system.root.model.geography.Location;
import org.cyk.system.root.model.globalidentification.GlobalIdentifier;
import org.cyk.system.root.model.mathematics.Average;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.root.model.mathematics.Rank;
import org.cyk.system.root.model.mathematics.Sort;
import org.cyk.system.root.model.party.Party;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.party.person.PersonExtendedInformations;
import org.cyk.system.root.model.party.person.PersonRelationship;
import org.cyk.system.root.model.party.person.PersonRelationshipType;
import org.cyk.system.root.model.party.person.Sex;
import org.cyk.system.root.model.security.Credentials;
import org.cyk.system.root.model.security.UserAccount;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.root.persistence.api.geography.ContactCollectionDao;
import org.cyk.system.root.persistence.api.party.person.PersonDao;
import org.cyk.system.root.persistence.api.party.person.PersonExtendedInformationsDao;
import org.cyk.system.root.persistence.api.party.person.PersonRelationshipDao;
import org.cyk.system.root.persistence.api.security.RoleDao;
import org.cyk.system.root.persistence.api.security.UserAccountDao;
import org.cyk.system.root.persistence.api.value.MeasureDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.actor.TeacherBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.EvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.persistence.api.actor.TeacherDao;
import org.cyk.system.school.persistence.api.session.ClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectDao;
import org.cyk.system.school.persistence.api.subject.StudentClassroomSessionDivisionSubjectDao;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.generator.AbstractGeneratable;
import org.cyk.utility.common.helper.StringHelper.CaseType;

import lombok.Getter;

@Singleton @Getter
public class IesaFakedDataProducer extends AbstractEnterpriseResourcePlanningFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;
	
	public IesaFakedDataProducer() {
		SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
		AbstractGeneratable.Listener.Adapter.Default.LOCALE = Locale.ENGLISH;
		RootConstant.Configuration.ReportTemplate.LOCALE = AbstractGeneratable.Listener.Adapter.Default.LOCALE;
		
		LanguageBusinessImpl.cache(Locale.FRENCH, "yes", null, CaseType.NONE, "yes");
		LanguageBusinessImpl.cache(Locale.FRENCH, "yes", null, CaseType.FURL, "yes");
		LanguageBusinessImpl.cache(Locale.FRENCH, "no", null, CaseType.FURL, "no");
		
		divisionOrderNumbers.add(1l);
		divisionOrderNumbers.add(2l);
		divisionOrderNumbers.add(3l);
		
		addClassroomSessionSuffixes(new Object[][]{
			{SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1,new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}}
			,{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,new String[]{null}}
			,{SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1,new String[]{null}}
		});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1
				,SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1}, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT}
				,{SchoolConstant.Code.Subject.CREATIVE_WRITING},{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE}
    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GRAMMAR},{SchoolConstant.Code.Subject.HANDWRITING}
    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.MATHEMATICS},{SchoolConstant.Code.Subject.PHONICS}
    			,{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.MORAL_EDUCATION}
    			,{SchoolConstant.Code.Subject.READING_COMPREHENSION},{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SCIENCE}
    			,{SchoolConstant.Code.Subject.UCMAS},{SchoolConstant.Code.Subject.SPELLING}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1
				,SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1}, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT}
				,{SchoolConstant.Code.Subject.COMPREHENSION},{SchoolConstant.Code.Subject.CREATIVE_WRITING}
    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE},{SchoolConstant.Code.Subject.GRAMMAR}
    			,{SchoolConstant.Code.Subject.HISTORY},{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE}
    			,{SchoolConstant.Code.Subject.MATHEMATICS},{SchoolConstant.Code.Subject.MORAL_EDUCATION},{SchoolConstant.Code.Subject.MUSIC}
    			,{SchoolConstant.Code.Subject.PHONICS},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.SCIENCE}
    			,{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SPELLING},{SchoolConstant.Code.Subject.UCMAS}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1}
				, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CHECKPOINT_MATHEMATICS}
				,{SchoolConstant.Code.Subject.CHECKPOINT_SCIENCES}
    			,{SchoolConstant.Code.Subject.COMPREHENSION},{SchoolConstant.Code.Subject.CREATIVE_WRITING},{SchoolConstant.Code.Subject.FRENCH}
    			,{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE},{SchoolConstant.Code.Subject.GRAMMAR},{SchoolConstant.Code.Subject.HISTORY}
    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE},{SchoolConstant.Code.Subject.MORAL_EDUCATION}
    			,{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.PHONICS},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION}
    			,{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SPELLING},{SchoolConstant.Code.Subject.UCMAS}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1}
				, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CHECKPOINT_ENGLISH_LEVEL}
				,{SchoolConstant.Code.Subject.CHECKPOINT_MATHEMATICS}
    			,{SchoolConstant.Code.Subject.CHECKPOINT_SCIENCES},{SchoolConstant.Code.Subject.DIVINITY},{SchoolConstant.Code.Subject.EARTH_SCIENCES}
    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GEOGRAPHY},{SchoolConstant.Code.Subject.HISTORY}
    			,{SchoolConstant.Code.Subject.HOME_ECONOMICS},{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH}
    			,{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.SOCIAL_STUDIES}
    			,{SchoolConstant.Code.Subject.SPANISH},{SchoolConstant.Code.Subject.STEM},{SchoolConstant.Code.Subject.UCMAS}});
		
		addClassroomSessionSubjects(new String[]{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1
				,SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1}
				, new String[][]{{SchoolConstant.Code.Subject.ACCOUNTING},{SchoolConstant.Code.Subject.ART_DESIGN},{SchoolConstant.Code.Subject.BIOLOGY}
    			,{SchoolConstant.Code.Subject.BUSINESS_STUDIES},{SchoolConstant.Code.Subject.CHEMISTRY},{SchoolConstant.Code.Subject.CREATIVITY_ACTIVITY_SERVICE}
    			,{SchoolConstant.Code.Subject.DEVELOPMENT_STUDIES},{SchoolConstant.Code.Subject.ECONOMICS},{SchoolConstant.Code.Subject.ENGLISH_FIRST_LANGUAGE}
    			,{SchoolConstant.Code.Subject.ENVIRONMENTAL_MANAGEMENT},{SchoolConstant.Code.Subject.EXTENDED_ESSAY},{SchoolConstant.Code.Subject.EXTENDED_MATHEMATICS}
    			,{SchoolConstant.Code.Subject.FRENCH_FOREIGN_LANGUAGE},{SchoolConstant.Code.Subject.GEOGRAPHY},{SchoolConstant.Code.Subject.HISTORY}
    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LAW},{SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH}
    			,{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.PHYSICS},{SchoolConstant.Code.Subject.SOCIOLOGY}
    			,{SchoolConstant.Code.Subject.SPANISH_FOREIGN_LANGUAGE},{SchoolConstant.Code.Subject.THEORY_OF_KNOWLEDGE}});
		
		addStudent(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1,null, new String[][]{ {"STUDPK","Bartheon","Robert"} });
		addStudent(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1,null, new String[][]{ {"STUDK1","Cecile","Jack"} });
		addStudent(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1,null, new String[][]{ {"STUDK2","Mamadou","kone"} });
		addStudent(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1,SchoolConstant.Code.ClassroomSessionSuffix.A, new String[][]{ {"STUDK3","Stack","ariel"} });
		
		addStudent(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.ClassroomSessionSuffix.A, new String[][]{ 
				{"STUDG1A","komenan","yao christian","kycdev@gmail.com","ckydevbackup@gmail.com"} 
				//,{"STUDG2A","yao","gérome"}
				//,{"STUDG3A","mamadi","issa"}
			});
		
		addStudent(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1,null, new String[][]{ {"STUDG8A","zadi","gérard"} });
		addStudent(SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,null, new String[][]{ {"STUDG9A","djedje","madi"} });
		
		addStudent(SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1,null, new String[][]{ {"STUDG12_1","yaya","diomande"} });
	}
	
	@Override
	protected void structure(Listener listener) {
		SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.TRUE;
		SchoolConstant.Configuration.Evaluation.SUM_ON_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT = Boolean.TRUE;
		inject(EvaluationTypeBusiness.class).delete(SchoolConstant.Code.EvaluationType.TEST);
		super.structure(listener);
		create(inject(TangibleProductBusiness.class).instanciateMany(new String[][]{{"TP01","Books Package Primary"},{"TP02", "Polo shirt Primary"}
		,{"TP03", "Sportswear Primary"},{"TP04","ID Card"},{"TP05","School Uniform (Up and Down) Primary"},{"TP06","Culottes Primary"}}));
		create(inject(IntangibleProductBusiness.class).instanciateMany(new String[][]{{"IP01","Re-registration"},{"IP02", "Tuition fees"},{"IP03", "Exam (STA)"}
			,{"IP04","UCMAS Program"},{"IP05","Swimming (First, Second & Third Terms)"},{"IP06","Art and Craft (First, Second & Third Terms)"}
			,{"IP07","Transportation (till June 2017)"}}));
		create(inject(SalableProductBusiness.class).instanciateMany(new String[][]{{"","","","","","","","","","","TP01","60000"}
			,{"","","","","","","","","","","TP02", "7000"},{"","","","","","","","","","","TP03", "7000"},{"","","","","","","","","","","TP04", "4000"}
			,{"","","","","","","","","","","TP05", "14000"},{"","","","","","","","","","","TP06", "7000"},{"","","","","","","","","","","IP01", "65000"}
			,{"","","","","","","","","","","IP02", "1450000"},{"","","","","","","","","","","IP03", "45000"},{"","","","","","","","","","","IP04", "40000"}
			,{"","","","","","","","","","","IP05", "30000"},{"","","","","","","","","","","IP06", "30000"},{"","","","","","","","","","","IP07", "30000"}}));
		
		createClassroomSessions();
		
	}
	
	@Override
	protected void doBusiness(Listener listener) {
		super.doBusiness(listener);
		createStudentClassroomSessions();
	}
	
	protected void createClassroomSessions(){
		Collection<ClassroomSession> classroomSessions = new ArrayList<>();
		String d = String.valueOf(63*inject(MeasureDao.class).read(RootConstant.Code.Measure.TIME_DAY).getValue().longValue());
		if(classroomSessionLevelTimeDivisionCodes.contains(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1))
			classroomSessions.add(inject(ClassroomSessionBusiness.class)
        		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
        		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
        		, new String[][]{}
        		, new String[][]{}
        		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS}
        			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}}));
       
		if(classroomSessionLevelTimeDivisionCodes.contains(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1))
			classroomSessions.add(inject(ClassroomSessionBusiness.class)
        		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
        		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
        		, new String[][]{}
        		, new String[][]{}
        		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_ENGLISH_LANGUAGE_ARTS_READING}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_COMMUNICATION_SKILLS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SCIENCE}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_STUDIES}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_MATHEMATICS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_WORK_HABITS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K1_STUDENT_SOCIAL_SKILLS}
        			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}}));
    	
		if(classroomSessionLevelTimeDivisionCodes.contains(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1))
			classroomSessions.add(inject(ClassroomSessionBusiness.class)
        		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
        		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
        		, new String[][]{}
        		, new String[][]{}
        		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION}
        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS}
        			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}}));
    	
		if(classroomSessionLevelTimeDivisionCodes.contains(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1))
			for(String suffix : getClassroomSessionSuffixes(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1)){
				classroomSessions.add(inject(ClassroomSessionBusiness.class)
	        		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1, suffix,null, RootConstant.Code.TimeDivisionType.TRIMESTER
	        		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
	        		, new String[][]{}
	        		, new String[][]{}
	        		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING_READINESS}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_READING}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WRITING}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_LISTENING_SPEAKING_VIEWING}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ALPHABET_IDENTIFICATION}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MATHEMATICS}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_SCIENCE_SOCIAL_STUDIES_MORAL_EDUCATION}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_ART_CRAFT}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_MUSIC}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_PHYSICAL_EDUCATION}
	        			,{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_K2_STUDENT_WORK_BEHAVIOUR_HABITS}
	        			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_KINDERGARTEN_STUDENT}}));
			}
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1}){
	    	for(String suffix : getClassroomSessionSuffixes(code)){
	    		if(classroomSessionLevelTimeDivisionCodes.contains(code))
	    			classroomSessions.add(inject(ClassroomSessionBusiness.class)
			    		.instanciateOne(code, suffix,null, RootConstant.Code.TimeDivisionType.TRIMESTER
			    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
			    		, getClassroomSessionSubjects(code)
			    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
			    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
			    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
			    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
			    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
	    		
	    	}
    	}
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1})
	    	for(String suffix : getClassroomSessionSuffixes(code))
	    		if(classroomSessionLevelTimeDivisionCodes.contains(code))
		    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
			    		.instanciateOne(code, suffix,null, RootConstant.Code.TimeDivisionType.TRIMESTER
			    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
			    		, getClassroomSessionSubjects(code)
			    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
			    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
			    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
			    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
			    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1})
    		if(classroomSessionLevelTimeDivisionCodes.contains(code))
	    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
		    		.instanciateOne(code, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
		    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
		    		, getClassroomSessionSubjects(code)
		    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
		    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
		    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
		    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
		    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1})
    		if(classroomSessionLevelTimeDivisionCodes.contains(code))
	    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
		    		.instanciateOne(code, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
		    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
		    		, getClassroomSessionSubjects(code)
		    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
		    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
		    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}
		    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
		    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1
    			,SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1})
    		if(classroomSessionLevelTimeDivisionCodes.contains(code))
	    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
		    		.instanciateOne(code, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
		    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
		    		, getClassroomSessionSubjects(code)
		    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
		    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
		    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}
		    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
		    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	System.out.println("Number of classroom session to create : "+classroomSessions.size());
    	create(classroomSessions);
	}
		
	protected void createStudentClassroomSessions(){
		Collection<StudentClassroomSession> studentClassroomSessions = new ArrayList<>();
		for(String levelTimeDivisionCode : classroomSessionLevelTimeDivisionCodes){
			for(String classroomSessionSuffixCode : getClassroomSessionSuffixes(levelTimeDivisionCode)){
				ClassroomSession classroomSession = inject(ClassroomSessionBusiness.class)
						.findInCurrentAcademicSessionByLevelTimeDivisionBySuffix(levelTimeDivisionCode,classroomSessionSuffixCode);
				
				Object[][] students = getStudents(levelTimeDivisionCode, classroomSessionSuffixCode);
				if(students!=null){
					for(Object[] studentInfos : students){
						Student student = inject(StudentBusiness.class).instanciateOneRandomly((String)studentInfos[0]);
						student.setName((String)studentInfos[1]);
						student.getPerson().setLastnames((String)studentInfos[2]);
						//student.getPerson().getContactCollection()!=null && student.getPerson().getContactCollection()
						//		.getElectronicMails()!=null);
				    	student.getPerson().getContactCollection().getElectronicMails().clear();
				    	/*if(studentInfos.length>3){
				    		inject(ElectronicMailBusiness.class).setAddress(student.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER, (String)studentInfos[3]);
				    		if(studentInfos.length>4){
				    			inject(ElectronicMailBusiness.class).setAddress(student.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER, (String)studentInfos[4]);
				    		}
				    	}*/
				    	
				    	StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{null,classroomSession.getCode()});
				    	studentClassroomSession.setStudent(student);
				    	studentClassroomSessions.add(studentClassroomSession);
				    	
					}		
				}
				
			}
			
		}
		System.out.println("Number of student classroom session to create : "+studentClassroomSessions.size());
    	create(studentClassroomSessions);
	}
	
	public Collection<Object[]> generate(){
		Collection<Object[]> datas = new ArrayList<>();
		for(String levelTimeDivisionCode : classroomSessionLevelTimeDivisionCodes){
			for(String suffixCode : getClassroomSessionSuffixes(levelTimeDivisionCode)){
				for(Long divisionOrderNumber : divisionOrderNumbers){
					datas.add(new Object[]{levelTimeDivisionCode,suffixCode,divisionOrderNumber});
				}
			}
		}
		return datas;
	}

	@Override
	protected void synchronize(Listener listener) {
		super.synchronize(listener);
		File directory = new File(System.getProperty("user.dir")+"\\src\\test\\resources\\data\\iesa");
		File file = new File(directory, "2016_2017_Trimester_1.xlsx");
		
		/*synchronizeFile(file);genericBusiness.flushEntityManager();
    	synchronizePerson(file);genericBusiness.flushEntityManager();
    	synchronizePersonRelationship(file);genericBusiness.flushEntityManager();
    	synchronizeUserAccount(file);genericBusiness.flushEntityManager();
    	synchronizeTeacher(file);genericBusiness.flushEntityManager();
    	*/
    	/*
    	synchronizeClassroomSession(file);genericBusiness.flushEntityManager();
    	synchronizeClassroomSessionDivisionSubject(file);genericBusiness.flushEntityManager();
    	*/
		
    	//synchronizeStudent(file);genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"PK");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"K1");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"K2");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"K3");genericBusiness.flushEntityManager();
		//synchronizeStudentClassroomSession(file,"G1");genericBusiness.flushEntityManager();
		/*synchronizeStudentClassroomSession(file,"G2");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G3");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G4");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G5");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G6");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G7");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G8");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G9");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G10");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G11");genericBusiness.flushEntityManager();
		synchronizeStudentClassroomSession(file,"G12");genericBusiness.flushEntityManager();
		*/
    	//synchronizeStudentClassroomSessionDivision(file);genericBusiness.flushEntityManager();
    	//synchronizeStudentClassroomSessionDivisionSubject(file);
    	
	}
	
	private void synchronizeFile(File file){
		final File directory = new File("C:\\cyk_iesaschool_db_files");
		IdentifiableExcelSheetReader<org.cyk.system.root.model.file.File> excelSheetReader = new IdentifiableExcelSheetReader<org.cyk.system.root.model.file.File>(file,org.cyk.system.root.model.file.File.class);
		
    	OneDimensionObjectArray<org.cyk.system.root.model.file.File> setter =
    			new OneDimensionObjectArray<org.cyk.system.root.model.file.File>(org.cyk.system.root.model.file.File.class){
    		
    	};
		
    	setter.addFieldNames(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_IDENTIFIER)
				,commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE)
				,org.cyk.system.root.model.file.File.FIELD_EXTENSION,org.cyk.system.root.model.file.File.FIELD_MIME
				,org.cyk.system.root.model.file.File.FIELD_URI,org.cyk.system.root.model.file.File.FIELD_REPRESENTATION_TYPE);
    	
		TwoDimensionObjectArray<org.cyk.system.root.model.file.File> twoDimensionObjectArray 
			= new TwoDimensionObjectArray<org.cyk.system.root.model.file.File>(setter){
				private static final long serialVersionUID = 1L; 
			
				@Override
				public org.cyk.system.root.model.file.File instanciate(Object[] values) {
					if("pdf".equalsIgnoreCase((String)values[2]))
						return null;
					org.cyk.system.root.model.file.File file = super.instanciate(values);
					try {
						file.setBytes(IOUtils.toByteArray(new FileInputStream(new File(directory,((String)values[6])+"."+((String)values[2])))));
					} catch (Exception e) {
						e.printStackTrace();
					}
					return file;
				}
				
				
				
		};
		inject(FileBusiness.class).synchronize(excelSheetReader, twoDimensionObjectArray);
	}
	 
	private void synchronizePerson(File file){
		IdentifiableExcelSheetReader<Person> excelSheetReader = new IdentifiableExcelSheetReader<Person>(file,Person.class);
    	
    	OneDimensionObjectArray<Person> setter = new OneDimensionObjectArray<Person>(Person.class){
			private static final long serialVersionUID = 1L;
			
			@Override
			public Object getValue(Class<?> clazz, Object value) {
				if(Sex.class.equals(clazz))
					if("M".equals(value))
						value = org.cyk.system.root.model.RootConstant.Code.Sex.MALE;
					else if("F".equals(value))
						value = org.cyk.system.root.model.RootConstant.Code.Sex.FEMALE;
				return super.getValue(clazz, value);
			}
			
		};
		
		setter.addFieldNames(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE) 
				,commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_NAME)
				,Person.FIELD_LASTNAMES
				,commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_EXISTENCE_PERIOD,Period.FIELD_FROM_DATE)
				,commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_BIRTH_LOCATION,AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER
						,GlobalIdentifier.FIELD_OTHER_DETAILS)
				,Person.FIELD_SEX
				,commonUtils.attributePath(Person.FIELD_EXTENDED_INFORMATIONS, PersonExtendedInformations.FIELD_TITLE)
				).addFieldName(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_IMAGE), 8)
				.addFieldName(commonUtils.attributePath(Person.FIELD_EXTENDED_INFORMATIONS, PersonExtendedInformations.FIELD_SIGNATURE_SPECIMEN), 9);
		
		TwoDimensionObjectArray<Person> twoDimensionObjectArray = new TwoDimensionObjectArray<Person>(setter){
			private static final long serialVersionUID = 1L;

			@Override
			public Person instanciate(Object[] values) {
				Person person = super.instanciate(values);
				person.setExtendedInformations(new PersonExtendedInformations(person));
				person.setBirthLocation((Location) inject(LocationBusiness.class).instanciateOne());
				if(StringUtils.isNotBlank((String)values[7])){
					if(person.getContactCollection()==null)
						person.setContactCollection(new ContactCollection());
					if(person.getContactCollection().getElectronicMails()==null)
						person.getContactCollection().setElectronicMails(new ArrayList<ElectronicMail>());
					person.getContactCollection().getElectronicMails().clear();
					person.getContactCollection().getElectronicMails().add(new ElectronicMail((String)values[7]));
				}
				return person;
			}
			
			@Override
			public Person getInstanceByKey(Object[] values, Object key, Object type) {
				Person person = super.getInstanceByKey(values, key, type);
				person.setExtendedInformations(inject(PersonExtendedInformationsDao.class).readByParty(person));
				inject(ContactCollectionBusiness.class).load(person.getContactCollection());
				return person;
			}
			
			@Override
			public Boolean getIgnoreExistingKey(Object[] values, Object key, Object keyType) {
				return Boolean.FALSE;
			}
		};
		
		inject(PersonBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	private void synchronizePersonRelationship(File file){
		IdentifiableExcelSheetReader<PersonRelationship> excelSheetReader = new IdentifiableExcelSheetReader<PersonRelationship>(file,PersonRelationship.class);
    	
    	OneDimensionObjectArray<PersonRelationship> setter = new OneDimensionObjectArray<PersonRelationship>(PersonRelationship.class){
			private static final long serialVersionUID = 1L;

			@Override
			public Object getValue(Class<?> fieldType, Object value) {
				if(PersonRelationshipType.class.equals(fieldType))
					value = StringUtils.replace((String)value, "_", "");
				return super.getValue(fieldType, value);
			}
			
		};
		
		//setter.addFieldName(PersonRelationship.FIELD_PERSON1, 0).addFieldName(PersonRelationship.FIELD_TYPE,1).addFieldName(PersonRelationship.FIELD_PERSON2, 2);
		
		TwoDimensionObjectArray<PersonRelationship> twoDimensionObjectArray = new TwoDimensionObjectArray<PersonRelationship>(setter){
			private static final long serialVersionUID = 1L;
			/*
			@Override
			public PersonRelationship getInstanceByKey(Object[] values, Object key, Object type) {
				return inject(PersonRelationshipDao.class).readByPerson1ByTypeByPerson2((Person)getOneDimension().getValue(Person.class, values[0])
						, (PersonRelationshipType)getOneDimension().getValue(PersonRelationshipType.class, values[1])
						, (Person)getOneDimension().getValue(Person.class, values[2]));
			}
			*/
		};
		
		inject(PersonRelationshipBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	private void synchronizeUserAccount(File file){
		IdentifiableExcelSheetReader<UserAccount> excelSheetReader = new IdentifiableExcelSheetReader<UserAccount>(file,UserAccount.class);
    	
    	OneDimensionObjectArrayAdapter<UserAccount> setter = new OneDimensionObjectArrayAdapter<UserAccount>(UserAccount.class){
			private static final long serialVersionUID = 1L;

			@Override
			public Object getValue(Class<?> fieldType, Object value) {
				if(Party.class.equals(fieldType))
					return inject(PersonDao.class).read((String)value);
				return super.getValue(fieldType, value);
			}
			
		};
		
		setter.addFieldName(UserAccount.FIELD_USER, 0).addFieldName(commonUtils.attributePath(UserAccount.FIELD_CREDENTIALS, Credentials.FIELD_USERNAME) ,1)
			.addFieldName(commonUtils.attributePath(UserAccount.FIELD_CREDENTIALS, Credentials.FIELD_PASSWORD), 2);
		
		TwoDimensionObjectArray<UserAccount> twoDimensionObjectArray = new TwoDimensionObjectArray<UserAccount>(setter){
			private static final long serialVersionUID = 1L;
			/*
			@Override
			public UserAccount getInstanceByKey(Object[] values, Object key, Object type) {
				return inject(UserAccountDao.class).readByCredentials(new Credentials((String)values[1], (String)values[2])); 
			}
			*/
			@Override
			public UserAccount instanciate(Object[] values) {
				UserAccount instance = inject(UserAccountBusiness.class).instanciateOne((String)values[0],(String)values[1], (String)values[2]);
				instance.getRoles().add(inject(RoleDao.class).read("USER"));
				instance.getRoles().add(inject(RoleDao.class).read("TEACHER"));
				return instance;
			}
		};
		
		inject(UserAccountBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	private void synchronizeStudent(File file){
		IdentifiableExcelSheetReader<Student> excelSheetReader = new IdentifiableExcelSheetReader<Student>(file,Student.class);
    	/*excelSheetReader.setRowCount(10);
    	excelSheetReader.setHasPrimaryKey(Boolean.TRUE);
    	excelSheetReader.setPrimaryKeyColumnIndexes(new LinkedHashSet<>(Arrays.asList(0)));
    	*/
		/*
		CompleteActorInstanciationOfManyFromValuesArguments<Student> completeStudentInstanciationOfManyFromValuesArguments = new CompleteActorInstanciationOfManyFromValuesArguments<Student>();
		
		completeStudentInstanciationOfManyFromValuesArguments.getInstanciationOfOneFromValuesArguments().setPersonCodeColumnIndex(0);
		
		completeStudentInstanciationOfManyFromValuesArguments.getInstanciationOfOneFromValuesArguments().setListener(new CompleteInstanciationOfOneFromValuesListener<Student>() {
			@Override
			public void beforeProcessing(Student student,String[] values) {}
    		@Override
			public void afterProcessing(Student student,String[] values) {
    			String[] p = StringUtils.split(values[1], Constant.CHARACTER_UNDESCORE.toString());
    			String classroomSessionCode = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING);
				String studentClassroomSessionCode = student.getCode()+classroomSessionCode;
				StudentClassroomSession studentClassroomSession = inject(StudentClassroomSessionDao.class).read(studentClassroomSessionCode);
				if(studentClassroomSession==null){
					studentClassroomSession = inject(StudentClassroomSessionBusiness.class).instanciateOne(student, inject(ClassroomSessionDao.class).read(classroomSessionCode));
					//System.out.println(StringUtils.join(p,"|")+";"+student.getIdentifier()+" : "+student.getCode()+" : "+classroomSessionCode+" : "+studentClassroomSession.getClassroomSession());
					student.setStudentClassroomSession(studentClassroomSession);
				}
			}
    		
		});
		inject(StudentBusiness.class).synchronize(excelSheetReader, completeStudentInstanciationOfManyFromValuesArguments);
		*/
		
		OneDimensionObjectArrayAdapter<Student> setter = new OneDimensionObjectArrayAdapter<Student>(Student.class);
    	setter.addFieldNames(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE));
    	TwoDimensionObjectArray<Student> twoDimensionObjectArray = new TwoDimensionObjectArray<Student>(setter){
			private static final long serialVersionUID = 1L;
    		
			@Override
			public Student instanciate(Object[] values) {
				Student student = super.instanciate(values);
				student.setPerson(inject(PersonDao.class).read((String) values[0]));
				return student;
			}
			
    	};
		inject(StudentBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	
	
	private void synchronizeStudentClassroomSession(File file,final String levelTimeDivisionCode){
		IdentifiableExcelSheetReader<StudentClassroomSession> excelSheetReader = new IdentifiableExcelSheetReader<StudentClassroomSession>(file,StudentClassroomSession.class);
		
		OneDimensionObjectArray<StudentClassroomSession> setter = new OneDimensionObjectArray<StudentClassroomSession>(StudentClassroomSession.class){
			private static final long serialVersionUID = 1L;

			/*@Override
    		public Object getKeyValue(Object[] values) {
    			String[] p = StringUtils.split((String)values[1], Constant.CHARACTER_UNDESCORE.toString());
    			String classroomSessionDivisionCode = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING)+"TRIMESTER1";
				String studentClassroomSessionDivisionCode = values[0]+classroomSessionDivisionCode;
    			return studentClassroomSessionDivisionCode;
    		}*/
			
			@Override
			public Object getValue(Class<?> fieldType, Object value) {
				if(ClassroomSession.class.equals(fieldType)){
					String[] p = StringUtils.split((String)value, Constant.CHARACTER_UNDESCORE.toString());
					value = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING);
				}
				return super.getValue(fieldType, value);
			}
    	};
    	setter.addFieldNames(StudentClassroomSessionDivision.FIELD_STUDENT,StudentClassroomSession.FIELD_CLASSROOM_SESSION);
    	
    	TwoDimensionObjectArray<StudentClassroomSession> twoDimensionObjectArray = new TwoDimensionObjectArray<StudentClassroomSession>(setter){
			private static final long serialVersionUID = 7689846377395483927L;
			
			@Override
    		public StudentClassroomSession getInstanceByKey(Object[] values, Object key, Object type) {
				return inject(StudentClassroomSessionDao.class).readByStudentByClassroomSession((Student)getOneDimension().getValue(Student.class, (String)values[0])
    					, (ClassroomSession)getOneDimension().getValue(ClassroomSession.class, (String)values[1]));
    		}
			
			@Override
			public StudentClassroomSession getInstance(Object[] values) {
				String[] p = StringUtils.split((String)values[1], Constant.CHARACTER_UNDESCORE.toString());
				if( levelTimeDivisionCode.equals(p[0]) )
					return super.getInstance(values);
				return null;
			}
			
    	};
    	inject(StudentClassroomSessionBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	private void synchronizeStudentClassroomSessionDivision(File file){
		IdentifiableExcelSheetReader<StudentClassroomSessionDivision> excelSheetReader = new IdentifiableExcelSheetReader<StudentClassroomSessionDivision>(file,StudentClassroomSessionDivision.class);
		
		OneDimensionObjectArray<StudentClassroomSessionDivision> setter = 
    			new OneDimensionObjectArray<StudentClassroomSessionDivision>(StudentClassroomSessionDivision.class){
					private static final long serialVersionUID = 1L;
			
			@Override
			public Object getValue(Class<?> fieldType, Object value) {
				if(ClassroomSessionDivision.class.equals(fieldType)){
					String[] p = StringUtils.split((String)value, Constant.CHARACTER_UNDESCORE.toString());
					value = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING)+"TRIMESTER1";
				}else if(Interval.class.equals(fieldType)){
					value = RootConstant.Code.generate(getInstance().getClassroomSessionDivision().getClassroomSession()
						.getAcademicSession().getNodeInformations().getStudentClassroomSessionDivisionAverageScale(), (String)value);
				}
				return super.getValue(fieldType, value);
			}
    	};
    	setter.addFieldNames(StudentClassroomSessionDivision.FIELD_STUDENT,StudentClassroomSessionDivision.FIELD_CLASSROOM_SESSION_DIVISION
    			,commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_AVERAGE,Average.FIELD_VALUE)
    			,commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_RANK,Rank.FIELD_VALUE)
    			,commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_RANK,Rank.FIELD_EXAEQUO)
    			,commonUtils.attributePath(StudentClassroomSessionDivision.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_AVERAGE_APPRECIATED_INTERVAL)
    			);
    	TwoDimensionObjectArray<StudentClassroomSessionDivision> twoDimensionObjectArray = new TwoDimensionObjectArray<StudentClassroomSessionDivision>(setter){
			private static final long serialVersionUID = 7689846377395483927L;
			@Override
    		public StudentClassroomSessionDivision getInstanceByKey(Object[] values, Object key, Object type) {
    			return inject(StudentClassroomSessionDivisionDao.class).readByStudentByClassroomSessionDivision((Student)getOneDimension().getValue(Student.class, (String)values[0])
    					, (ClassroomSessionDivision)getOneDimension().getValue(ClassroomSessionDivision.class, (String)values[1]));
    		}
			
			@Override
			public StudentClassroomSessionDivision instanciate(Object[] values) {
				StudentClassroomSessionDivision studentClassroomSessionDivision = super.instanciate(values);
				if(studentClassroomSessionDivision.getClassroomSessionDivision()==null)
					return null;
				return studentClassroomSessionDivision;
			}
			
    	};
    	inject(StudentClassroomSessionDivisionBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	private void synchronizeStudentClassroomSessionDivisionSubject(File file){
		for(int i = 0 ; i < 500 ; i = i + 500){
			IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubject> excelSheetReader = new IdentifiableExcelSheetReader<StudentClassroomSessionDivisionSubject>(file,StudentClassroomSessionDivisionSubject.class);
			excelSheetReader.setSheetName("StudentClassroomSDS");
			excelSheetReader.setFromRowIndex(i);
			excelSheetReader.setRowCount(500);
			OneDimensionObjectArray<StudentClassroomSessionDivisionSubject> setter = 
	    			new OneDimensionObjectArray<StudentClassroomSessionDivisionSubject>(StudentClassroomSessionDivisionSubject.class){
						private static final long serialVersionUID = 1L;
	
				@Override
				public Object getValue(Class<?> fieldType, Object value) {
					if(ClassroomSessionDivisionSubject.class.equals(fieldType)){
						String[] p = StringUtils.split(StringUtils.substringBefore((String)value, "_TRIMESTER_") , Constant.CHARACTER_UNDESCORE.toString());
						value = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING)+"TRIMESTER1"
								+StringUtils.substringAfter((String)value, "_TRIMESTER_");
					}else if(Interval.class.equals(fieldType)){
						value = RootConstant.Code.generate(getInstance().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession()
							.getAcademicSession().getNodeInformations().getStudentSubjectAverageScale(), (String)value);
					}
					return super.getValue(fieldType, value);
				}
				
	    	};
	    	setter.addFieldNames(StudentClassroomSessionDivisionSubject.FIELD_STUDENT,StudentClassroomSessionDivisionSubject.FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT
	    			,commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_AVERAGE,Average.FIELD_VALUE)
	    			,commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_RANK,Rank.FIELD_VALUE)
	    			,commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_RANK,Rank.FIELD_EXAEQUO)
	    			,commonUtils.attributePath(StudentClassroomSessionDivisionSubject.FIELD_RESULTS, StudentResults.FIELD_EVALUATION_SORT,Sort.FIELD_AVERAGE_APPRECIATED_INTERVAL)
	    			);
	    	TwoDimensionObjectArray<StudentClassroomSessionDivisionSubject> twoDimensionObjectArray = new TwoDimensionObjectArray<StudentClassroomSessionDivisionSubject>(setter){
				private static final long serialVersionUID = 7689846377395483927L;
				@Override
	    		public StudentClassroomSessionDivisionSubject getInstanceByKey(Object[] values, Object key, Object type) {
					return inject(StudentClassroomSessionDivisionSubjectDao.class).readByStudentBySubject((Student)getOneDimension().getValue(Student.class, (String)values[0])
	    					, (ClassroomSessionDivisionSubject)getOneDimension().getValue(ClassroomSessionDivisionSubject.class, (String)values[1]));
	    		}
				
				@Override
				public StudentClassroomSessionDivisionSubject instanciate(Object[] values) {
					StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject =  super.instanciate(values);
					if(studentClassroomSessionDivisionSubject.getClassroomSessionDivisionSubject()==null)
						return null;
					return studentClassroomSessionDivisionSubject;
				}
	    	};
	    	inject(StudentClassroomSessionDivisionSubjectBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
		}
	}
	
	private void synchronizeTeacher(File file){
		IdentifiableExcelSheetReader<Teacher> excelSheetReader = new IdentifiableExcelSheetReader<Teacher>(file,Teacher.class);
    	OneDimensionObjectArray<Teacher> setter = new OneDimensionObjectArray<Teacher>(Teacher.class);
    	setter.addFieldNames(commonUtils.attributePath(AbstractIdentifiable.FIELD_GLOBAL_IDENTIFIER, GlobalIdentifier.FIELD_CODE));
    	TwoDimensionObjectArray<Teacher> twoDimensionObjectArray = new TwoDimensionObjectArray<Teacher>(setter){
			private static final long serialVersionUID = 1L;
    		
			@Override
			public Teacher instanciate(Object[] values) {
				Teacher teacher = super.instanciate(values);
				teacher.setPerson(inject(PersonDao.class).read((String) values[0]));
				if(teacher.getPerson()==null)
					return null;
				return teacher;
			}
			
    	};
    	inject(TeacherBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
	}
	
	private void synchronizeClassroomSession(File file){
		IdentifiableExcelSheetReader<ClassroomSession> excelSheetReader = new IdentifiableExcelSheetReader<ClassroomSession>(file,ClassroomSession.class);
    	
    	OneDimensionObjectArrayAdapter<ClassroomSession> setter = new OneDimensionObjectArrayAdapter<ClassroomSession>(ClassroomSession.class);
		
		TwoDimensionObjectArray<ClassroomSession> twoDimensionObjectArray = new TwoDimensionObjectArray<ClassroomSession>(setter){
			private static final long serialVersionUID = 1L;
			@Override
			public ClassroomSession getInstanceByKey(Object[] values, Object key, Object type) {
				String[] p = StringUtils.split((String)values[0], Constant.CHARACTER_UNDESCORE.toString());
    			String classroomSessionCode = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING);
				ClassroomSession classroomSession = inject(ClassroomSessionDao.class).read(classroomSessionCode);
				classroomSession.setCoordinator(inject(TeacherDao.class).read((String)values[1]));
				return classroomSession;
			}
		};
		
		inject(ClassroomSessionBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
    	
	}
	
	private void synchronizeClassroomSessionDivisionSubject(File file){
		IdentifiableExcelSheetReader<ClassroomSessionDivisionSubject> excelSheetReader = new IdentifiableExcelSheetReader<ClassroomSessionDivisionSubject>(file,ClassroomSessionDivisionSubject.class);
		
    	OneDimensionObjectArrayAdapter<ClassroomSessionDivisionSubject> setter = new OneDimensionObjectArrayAdapter<ClassroomSessionDivisionSubject>(ClassroomSessionDivisionSubject.class);
		
		TwoDimensionObjectArray<ClassroomSessionDivisionSubject> twoDimensionObjectArray = new TwoDimensionObjectArray<ClassroomSessionDivisionSubject>(setter){
			private static final long serialVersionUID = 1L;
			
			@Override
			public ClassroomSessionDivisionSubject instanciate(Object[] values) {
				return null;
			}
			
			@Override
			public ClassroomSessionDivisionSubject getInstanceByKey(Object[] values, Object key, Object type) {
				String[] p = StringUtils.split((String)values[0], Constant.CHARACTER_UNDESCORE.toString());
				String subjectCode = StringUtils.substringAfter((String)values[2], "TRIMESTER_");
    			String classroomSessionDivisionSubjectCode = "04/10/200018/06/2001"+p[0]+"YEAR1"+(p.length > 1 ? p[1] : Constant.EMPTY_STRING)+"TRIMESTER1"+subjectCode;
				
    			ClassroomSessionDivisionSubject classroomSessionDivisionSubject = inject(ClassroomSessionDivisionSubjectDao.class).read(classroomSessionDivisionSubjectCode);
				if(classroomSessionDivisionSubject!=null){
					classroomSessionDivisionSubject.setTeacher(inject(TeacherDao.class).read((String)values[3]));
				}else{
					System.out.println(classroomSessionDivisionSubjectCode+" : "+subjectCode+" : ");	
				}
				return classroomSessionDivisionSubject;
			}
		};
		
		inject(ClassroomSessionDivisionSubjectBusiness.class).synchronize(excelSheetReader,twoDimensionObjectArray);
    	
	}

}
