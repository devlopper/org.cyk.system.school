package org.cyk.system.school.business.impl.iesa;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.system.company.business.api.product.IntangibleProductBusiness;
import org.cyk.system.company.business.api.product.TangibleProductBusiness;
import org.cyk.system.company.business.api.sale.SalableProductBusiness;
import org.cyk.system.company.model.CompanyConstant;
import org.cyk.system.root.business.impl.PersistDataListener;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.value.MeasureDao;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.impl._dataproducer.AbstractEnterpriseResourcePlanningFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.generator.AbstractGeneratable;

import lombok.Getter;

@Getter
public abstract class AbstractIesaFakedDataProducer extends AbstractEnterpriseResourcePlanningFakedDataProducer implements Serializable {

	private static final long serialVersionUID = -1832900422621121762L;
		
	@Override
	protected void structure() {
		AbstractGeneratable.Listener.Adapter.Default.LOCALE = Locale.ENGLISH;
    	PersistDataListener.COLLECTION.add(new PersistDataListener.Adapter.Default(){
			private static final long serialVersionUID = -950053441831528010L;
			@SuppressWarnings("unchecked")
			@Override
			public <T> T processPropertyValue(Class<?> aClass,String instanceCode, String name, T value) {
				if(ArrayUtils.contains(new String[]{CompanyConstant.Code.File.DOCUMENT_HEADER}, instanceCode)){
					if(PersistDataListener.RELATIVE_PATH.equals(name))
						return (T) "/report/iesa/salecashregistermovementlogo.png";
				}
				return super.processPropertyValue(aClass, instanceCode, name, value);
			}
		});
		super.structure();
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
		
		school();
	}
	
	private void school(){
		Collection<ClassroomSession> classroomSessions = new ArrayList<>();
		String d = String.valueOf(63*inject(MeasureDao.class).read(RootConstant.Code.Measure.TIME_DAY).getValue().longValue());
		long t = System.currentTimeMillis();
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
    	
		classroomSessions.add(inject(ClassroomSessionBusiness.class)
        		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
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
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1})
	    	for(String suffix : new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B}){
	    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
		    		.instanciateOne(code, suffix,null, RootConstant.Code.TimeDivisionType.TRIMESTER
		    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
		    		, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CREATIVE_WRITING},{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE}
		    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GRAMMAR},{SchoolConstant.Code.Subject.HANDWRITING}
		    			/*,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.MATHEMATICS},{SchoolConstant.Code.Subject.PHONICS}
		    			,{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.MORAL_EDUCATION}
		    			,{SchoolConstant.Code.Subject.READING_COMPREHENSION},{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SCIENCE}
		    			,{SchoolConstant.Code.Subject.UCMAS},{SchoolConstant.Code.Subject.SPELLING}*/}
		    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
		    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
		    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
		    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
		    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
	    		
	    	}
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1})
	    	for(String suffix : new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A,SchoolConstant.Code.ClassroomSessionSuffix.B})
	    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
		    		.instanciateOne(code, suffix,null, RootConstant.Code.TimeDivisionType.TRIMESTER
		    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
		    		, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.COMPREHENSION},{SchoolConstant.Code.Subject.CREATIVE_WRITING}
		    			,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE},{SchoolConstant.Code.Subject.GRAMMAR}
		    			/*,{SchoolConstant.Code.Subject.HISTORY},{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE}
		    			,{SchoolConstant.Code.Subject.MATHEMATICS},{SchoolConstant.Code.Subject.MORAL_EDUCATION},{SchoolConstant.Code.Subject.MUSIC}
		    			,{SchoolConstant.Code.Subject.PHONICS},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.SCIENCE}
		    			,{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SPELLING},{SchoolConstant.Code.Subject.UCMAS}*/}
		    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
		    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
		    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
		    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
		    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1})
    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
	    		.instanciateOne(code, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
	    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
	    		, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CHECKPOINT_MATHEMATICS},{SchoolConstant.Code.Subject.CHECKPOINT_SCIENCES}
	    			,{SchoolConstant.Code.Subject.COMPREHENSION},{SchoolConstant.Code.Subject.CREATIVE_WRITING},{SchoolConstant.Code.Subject.FRENCH}
	    			/*,{SchoolConstant.Code.Subject.GENERAL_KNOWLEDGE},{SchoolConstant.Code.Subject.GRAMMAR},{SchoolConstant.Code.Subject.HISTORY}
	    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE},{SchoolConstant.Code.Subject.MORAL_EDUCATION}
	    			,{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.PHONICS},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION}
	    			,{SchoolConstant.Code.Subject.SOCIAL_STUDIES},{SchoolConstant.Code.Subject.SPELLING},{SchoolConstant.Code.Subject.UCMAS}*/}
	    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
	    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
	    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
	    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
	    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1})
    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
	    		.instanciateOne(code, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
	    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
	    		, new String[][]{{SchoolConstant.Code.Subject.ART_CRAFT},{SchoolConstant.Code.Subject.CHECKPOINT_ENGLISH_LEVEL},{SchoolConstant.Code.Subject.CHECKPOINT_MATHEMATICS}
	    			,{SchoolConstant.Code.Subject.CHECKPOINT_SCIENCES},{SchoolConstant.Code.Subject.DIVINITY},{SchoolConstant.Code.Subject.EARTH_SCIENCES}
	    			/*,{SchoolConstant.Code.Subject.FRENCH},{SchoolConstant.Code.Subject.GEOGRAPHY},{SchoolConstant.Code.Subject.HISTORY}
	    			,{SchoolConstant.Code.Subject.HOME_ECONOMICS},{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH}
	    			,{SchoolConstant.Code.Subject.MUSIC},{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.SOCIAL_STUDIES}
	    			,{SchoolConstant.Code.Subject.SPANISH},{SchoolConstant.Code.Subject.STEM},{SchoolConstant.Code.Subject.UCMAS}*/}
	    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
	    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
	    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
	    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
	    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	for(String code : new String[]{SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1
    			,SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1,SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1})
    		classroomSessions.add(inject(ClassroomSessionBusiness.class)
	    		.instanciateOne(code, null,null, RootConstant.Code.TimeDivisionType.TRIMESTER
	    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
	    		, new String[][]{{SchoolConstant.Code.Subject.ACCOUNTING},{SchoolConstant.Code.Subject.ART_DESIGN},{SchoolConstant.Code.Subject.BIOLOGY}
	    			,{SchoolConstant.Code.Subject.BUSINESS_STUDIES},{SchoolConstant.Code.Subject.CHEMISTRY},{SchoolConstant.Code.Subject.CREATIVITY_ACTIVITY_SERVICE}
	    			/*,{SchoolConstant.Code.Subject.DEVELOPMENT_STUDIES},{SchoolConstant.Code.Subject.ECONOMICS},{SchoolConstant.Code.Subject.ENGLISH_FIRST_LANGUAGE}
	    			,{SchoolConstant.Code.Subject.ENVIRONMENTAL_MANAGEMENT},{SchoolConstant.Code.Subject.EXTENDED_ESSAY},{SchoolConstant.Code.Subject.EXTENDED_MATHEMATICS}
	    			,{SchoolConstant.Code.Subject.FRENCH_FOREIGN_LANGUAGE},{SchoolConstant.Code.Subject.GEOGRAPHY},{SchoolConstant.Code.Subject.HISTORY}
	    			,{SchoolConstant.Code.Subject.ICT_COMPUTER},{SchoolConstant.Code.Subject.LAW},{SchoolConstant.Code.Subject.LITERATURE_IN_ENGLISH}
	    			,{SchoolConstant.Code.Subject.PHYSICAL_EDUCATION},{SchoolConstant.Code.Subject.PHYSICS},{SchoolConstant.Code.Subject.SOCIOLOGY}
	    			,{SchoolConstant.Code.Subject.SPANISH_FOREIGN_LANGUAGE},{SchoolConstant.Code.Subject.THEORY_OF_KNOWLEDGE}*/}
	    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
	    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
	    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
	    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
	    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	System.out.println( (System.currentTimeMillis() - t) / 1000 ); t = System.currentTimeMillis();
    	System.out.println("Classrooms : "+classroomSessions.size());
    	create(classroomSessions);
    	System.out.println( (System.currentTimeMillis() - t) / 1000 );
	}

	@Override
	protected void doBusiness(Listener listener) {
		//super.doBusiness(listener);
	}
		
	
}
