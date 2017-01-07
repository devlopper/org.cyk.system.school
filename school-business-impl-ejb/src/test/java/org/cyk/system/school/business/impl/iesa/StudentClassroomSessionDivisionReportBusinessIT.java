package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.cyk.system.root.business.api.GenericBusiness;
import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.root.persistence.api.value.MeasureDao;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.CommonUtils;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
     
    @Override
    protected void businesses() {
    	String d = String.valueOf(63*inject(MeasureDao.class).read(RootConstant.Code.Measure.TIME_DAY).getValue().longValue());
    	ClassroomSession classroomSessionG1A = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
    		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, SchoolConstant.Code.ClassroomSessionSuffix.A,null, RootConstant.Code.TimeDivisionType.TRIMESTER
    		, new String[][]{{"1","1","1/1/2000","1/4/2000",d,"true","false"},{"2","1","1/5/2000","1/8/2000",d,"true","false"},{"3","1","1/9/2000","1/12/2000",d,"true","false"}}
    		, new String[][]{{SchoolConstant.Code.Subject.ACCOUNTING},{SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS},{SchoolConstant.Code.Subject.ART_CRAFT}}
    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"}
    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"}}
    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT}
    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	ClassroomSession classroomSessionG8A = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
    		.instanciateOne(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1, SchoolConstant.Code.ClassroomSessionSuffix.A,null, RootConstant.Code.TimeDivisionType.TRIMESTER
    				, new String[][]{{"1","1","1/1/2000","1/4/2000",d,"true","false"},{"2","1","1/5/2000","1/8/2000",d,"true","false"},{"3","1","1/9/2000","1/12/2000",d,"true","false"}}
    		, new String[][]{{SchoolConstant.Code.Subject.CHEMISTRY},{SchoolConstant.Code.Subject.BUSINESS_STUDIES},{SchoolConstant.Code.Subject.CHECKPOINT_ENGLISH_LEVEL}}
    		, new String[][]{{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
    			,{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.2","100"}
    			,{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.6","100"}}
    		, new String[][]{{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},{SchoolConstant.Code.MetricCollection.BEHAVIOUR_SECONDARY_STUDENT}
    			,{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT}}));
    	
    	
    	/*
    	ClassroomSession classroomSession = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
    		.instanciateOne(new String[]{null,SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1,"A",null,RootConstant.Code.TimeDivisionType.TRIMESTER
    		,StringUtils.join(new String[]{
    			StringUtils.join(new String[]{"1","1","1/1/2000","1/4/2000",d},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{"2","1","1/5/2000","1/8/2000",d},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{"3","1","1/9/2000","1/12/2000",d},Constant.CHARACTER_COMA.toString())
    			},Constant.CHARACTER_VERTICAL_BAR.toString())
    		
    		,StringUtils.join(new String[]{
    			StringUtils.join(new String[]{SchoolConstant.Code.Subject.ACCOUNTING},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{SchoolConstant.Code.Subject.ADVANCED_MATHEMATICS},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{SchoolConstant.Code.Subject.ART_CRAFT},Constant.CHARACTER_COMA.toString())
    		},Constant.CHARACTER_VERTICAL_BAR.toString())
    		
    		,StringUtils.join(new String[]{
    			StringUtils.join(new String[]{SchoolConstant.Code.EvaluationType.TEST1,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{SchoolConstant.Code.EvaluationType.TEST2,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.15","100"},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{SchoolConstant.Code.EvaluationType.EXAM,SchoolConstant.Code.Interval.EVALUATION_COUNT_BY_TYPE,"0.7","100"},Constant.CHARACTER_COMA.toString())
    			},Constant.CHARACTER_VERTICAL_BAR.toString())
    		
    		,StringUtils.join(new String[]{
				StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_STUDENT},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_PRIMARY_STUDENT},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.COMMUNICATION_STUDENT},Constant.CHARACTER_COMA.toString())
			},Constant.CHARACTER_VERTICAL_BAR.toString())
        		
    		
    		}));
    	*/
    	/*    	
    	ClassroomSession classroomSessionPk = (ClassroomSession) create(inject(ClassroomSessionBusiness.class)
    		.instanciateOne(new String[]{null,SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1,null,null,RootConstant.Code.TimeDivisionType.TRIMESTER
			,StringUtils.join(new String[]{
    			StringUtils.join(new String[]{"1","1","1/1/2000","1/4/2000",d},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{"2","1","1/5/2000","1/8/2000",d},Constant.CHARACTER_COMA.toString())
    			,StringUtils.join(new String[]{"3","1","1/9/2000","1/12/2000",d},Constant.CHARACTER_COMA.toString())
    			},Constant.CHARACTER_VERTICAL_BAR.toString())
    		    		
    		,StringUtils.join(new String[]{
    			
    		},Constant.CHARACTER_VERTICAL_BAR.toString())
    		
    		,StringUtils.join(new String[]{
    			
    		},Constant.CHARACTER_VERTICAL_BAR.toString())
    		
    		,StringUtils.join(new String[]{
				StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.ATTENDANCE_KINDERGARTEN_STUDENT},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_EXPRESSIVE_LANGUAGE},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_RECEPTIVE_LANGUAGE},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_READING_READNESS},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_NUMERACY_DEVELOPMENT},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_ARTS_MUSIC},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_SOCIAL_EMOTIONAL_DEVELOPMENT},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_GROSS_MOTOR_SKILLS},Constant.CHARACTER_COMA.toString())
				,StringUtils.join(new String[]{SchoolConstant.Code.MetricCollection.BEHAVIOUR_KINDERGARTEN_PK_STUDENT_FINE_MOTOR_SKILLS},Constant.CHARACTER_COMA.toString())
			},Constant.CHARACTER_VERTICAL_BAR.toString())
    			
    		}));
    	
    	String classroomSessionPkCode = classroomSessionPk.getCode();
    	*/
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
    	
    	Student pkStudent = inject(StudentBusiness.class).instanciateOneRandomly();
    	pkStudent.setCode("PK_STUD1");
    	pkStudent.setName("Zadi");
    	pkStudent.getPerson().setLastnames("leon");
    	//inject(ElectronicMailBusiness.class).setAddress(pkStudent.getPerson(), PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	//inject(ElectronicMailBusiness.class).setAddress(pkStudent.getPerson(), PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	/*
    	Student k1Student = inject(StudentBusiness.class).instanciateOne();
    	k1Student.setCode("K1_STUD1");
    	k1Student.setName("Kacou");
    	k1Student.getPerson().setLastnames("philipe");
    	
    	Student k2Student = inject(StudentBusiness.class).instanciateOne();
    	k2Student.setCode("K2_STUD1");
    	k2Student.setName("Anza");
    	k2Student.getPerson().setLastnames("roger");
    	
    	Student k3Student = inject(StudentBusiness.class).instanciateOne();
    	k3Student.setCode("K3_STUD1");
    	k3Student.setName("Aka");
    	k3Student.getPerson().setLastnames("clarisse");
    	*/
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(pkStudent/*,k1Student,k2Student,k3Student*/,studentG1A,studentG8A)
    			,AbstractIdentifiable.class));
    	
    	//create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{"PK_STUD1",classroomSessionPkCode}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentG1A.getCode(),classroomSessionG1A.getCode()}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{studentG8A.getCode(),classroomSessionG8A.getCode()}));
    	
    	/*schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{pkStudent.getCode()},dataProducer.getPk().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k1Student.getCode()},dataProducer.getK1().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k2Student.getCode()},dataProducer.getK2().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k3Student.getCode()},dataProducer.getK3().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	*/
    	/*schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{student1.getCode()},
    			inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix(SchoolConstant.Code.LevelName.G1,"A").iterator().next() , new Object[][]{{15},{15},{15}}); 
    	*/
    	
    	//schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.PK,null,1l}}, Boolean.TRUE, Boolean.FALSE);
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.G1,"A",1l}}, Boolean.TRUE, Boolean.FALSE);
    	//schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.G1,"A",1l}}, Boolean.FALSE, Boolean.FALSE);
    	//schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.G1,"A",1l}}, Boolean.FALSE, Boolean.FALSE);
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.G8,"A",1l}}, Boolean.TRUE, Boolean.FALSE);
    	
    	/*
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{
    		{SchoolConstant.Code.LevelName.PK,null,1l}
    		,{SchoolConstant.Code.LevelName.K1,null,1l}
    		,{SchoolConstant.Code.LevelName.K2,null,1l}
    		,{SchoolConstant.Code.LevelName.K3,null,1l}
    		,{SchoolConstant.Code.LevelName.G1,"A",1l}
    	}, Boolean.TRUE, Boolean.FALSE);
    	*/
    }
        
}
