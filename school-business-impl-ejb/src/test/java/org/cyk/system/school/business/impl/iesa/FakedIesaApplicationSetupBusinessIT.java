package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import javax.inject.Inject;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.value.MeasureDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.CommonUtils;

public class FakedIesaApplicationSetupBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    
    @Override
    protected void businesses() {
    	String d = String.valueOf(63*inject(MeasureDao.class).read(RootConstant.Code.Measure.TIME_DAY).getValue().longValue());
    	ClassroomSession classroomSessionPK1 = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
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
    	
    	ClassroomSession classroomSessionK1 = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
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
    	
    	ClassroomSession classroomSessionK2 = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
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
    	
    	ClassroomSession classroomSessionK3 = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
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
    	
    	ClassroomSession classroomSessionG1A = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
    		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, SchoolConstant.Code.ClassroomSessionSuffix.A,null, RootConstant.Code.TimeDivisionType.TRIMESTER
    		, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
    		, new String[][]{{SchoolConstant.Code.Subject.ACCOUNTING},{SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS},{SchoolConstant.Code.Subject.ART_CRAFT}}
    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	ClassroomSession classroomSessionG8A = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
    		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1, SchoolConstant.Code.ClassroomSessionSuffix.A,null, RootConstant.Code.TimeDivisionType.TRIMESTER
    				, new String[][]{{"1","1","1/1/2000 0:0","1/4/2000 0:0",d,"true","false"},{"2","1","1/5/2000 0:0","1/8/2000 0:0",d,"true","false"},{"3","1","1/9/2000 0:0","1/12/2000 0:0",d,"true","false"}}
    		, new String[][]{{SchoolConstant.Code.Subject.CHEMISTRY},{SchoolConstant.Code.Subject.BUSINESS_STUDIES},{SchoolConstant.Code.Subject.CHECKPOINT_ENGLISH_LEVEL}}
    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}
    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	Student studentG1A = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentG1A.setCode("STUDG1A");
    	studentG1A.setName("komenan");
    	studentG1A.getPerson().setLastnames("yao christian");
    	if(studentG1A.getPerson().getContactCollection()!=null && studentG1A.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentG1A.getPerson().getContactCollection().getElectronicMails().clear();
    	inject(ElectronicMailBusiness.class).setAddress(studentG1A.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	inject(ElectronicMailBusiness.class).setAddress(studentG1A.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
    	Student studentG8A = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentG8A.setCode("STUDG8A");
    	studentG8A.setName("zadi");
    	studentG8A.getPerson().setLastnames("gérard");
    	if(studentG8A.getPerson().getContactCollection()!=null && studentG8A.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentG8A.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentPK1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentPK1.setCode("STUDPK");
    	studentPK1.setName("Bartheon");
    	studentPK1.getPerson().setLastnames("Robert");
    	if(studentPK1.getPerson().getContactCollection()!=null && studentPK1.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentPK1.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentK1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentK1.setCode("STUDK1");
    	studentK1.setName("Cecile");
    	studentK1.getPerson().setLastnames("Jack");
    	if(studentK1.getPerson().getContactCollection()!=null && studentK1.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentK1.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentK2 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentK2.setCode("STUDK2");
    	studentK2.setName("Mamadou");
    	studentK2.getPerson().setLastnames("kone");
    	if(studentK2.getPerson().getContactCollection()!=null && studentK2.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentK2.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	Student studentK3 = inject(StudentBusiness.class).instanciateOneRandomly();
    	studentK3.setCode("STUDK3");
    	studentK3.setName("Stack");
    	studentK3.getPerson().setLastnames("ariel");
    	if(studentK3.getPerson().getContactCollection()!=null && studentK3.getPerson().getContactCollection().getElectronicMails()!=null)
    		studentK3.getPerson().getContactCollection().getElectronicMails().clear();
    	
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(studentPK1,studentK1,studentK2,studentK3,studentG1A,studentG8A)
    			,AbstractIdentifiable.class));
    	
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentPK1.getCode(),classroomSessionPK1.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentK1.getCode(),classroomSessionK1.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentK2.getCode(),classroomSessionK2.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentK3.getCode(),classroomSessionK3.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentG1A.getCode(),classroomSessionG1A.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentG8A.getCode(),classroomSessionG8A.getCode()}));
    	
    	
    	System.exit(0);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return iesaFakedDataProducer;
    }
        
}
