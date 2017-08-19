package org.cyk.system.school.business.impl._dataproducer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import lombok.Getter;

import org.cyk.system.company.business.api.product.IntangibleProductBusiness;
import org.cyk.system.company.business.api.product.TangibleProductBusiness;
import org.cyk.system.company.business.api.sale.SalableProductBusiness;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.value.MeasureDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.subject.EvaluationTypeBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;

@Getter
public abstract class AbstractEnterpriseResourcePlanningFakedDataProducer extends AbstractSchoolFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;
	
	public AbstractEnterpriseResourcePlanningFakedDataProducer() {
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
		
		addStudent(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.ClassroomSessionSuffix.B, new String[][]{ 
				{"STUDG1B","doudou","yves junior","doudou@gmail.com"} 
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
	
	public Collection<ClassroomSession> createClassroomSessions(){
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
    	//create(classroomSessions);
    	for(ClassroomSession classroomSession : classroomSessions)
    		create(classroomSession);
    	
    	return classroomSessions;
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
		
}
