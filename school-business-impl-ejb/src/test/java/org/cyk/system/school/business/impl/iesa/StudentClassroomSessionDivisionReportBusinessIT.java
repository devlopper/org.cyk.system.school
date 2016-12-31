package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
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
import org.cyk.utility.common.Constant;
import org.joda.time.DateTimeConstants;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void populate() {
    	super.populate();
    	//installApplication();
    }
    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	String d = String.valueOf(63*inject(MeasureDao.class).read(RootConstant.Code.Measure.TIME_DAY).getValue().longValue());
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
    	
    	String classroomSessionCode = classroomSession.getCode();
    	    	
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
    	
    	Student student1 = inject(StudentBusiness.class).instanciateOneRandomly();
    	student1.setCode("STUD1");
    	student1.setName("komenan");
    	student1.getPerson().setLastnames("yao christian");
    	if(student1.getPerson().getContactCollection()!=null && student1.getPerson().getContactCollection().getElectronicMails()!=null)
    		student1.getPerson().getContactCollection().getElectronicMails().clear();
    	inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
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
    	inject(GenericBusiness.class).create(CommonUtils.getInstance().castCollection(Arrays.asList(pkStudent/*,k1Student,k2Student,k3Student*/,student1),AbstractIdentifiable.class));
    	
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{"PK_STUD1",classroomSessionPkCode}));
    	create(inject(StudentClassroomSessionBusiness.class).instanciateOne(new String[]{"STUD1",classroomSessionCode}));
    	
    	/*schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{pkStudent.getCode()},dataProducer.getPk().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k1Student.getCode()},dataProducer.getK1().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k2Student.getCode()},dataProducer.getK2().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k3Student.getCode()},dataProducer.getK3().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	*/
    	/*schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{student1.getCode()},
    			inject(ClassroomSessionBusiness.class).findByLevelNameBySuffix(SchoolConstant.Code.LevelName.G1,"A").iterator().next() , new Object[][]{{15},{15},{15}}); 
    	*/
    	
    	//schoolBusinessTestHelper.getEvaluationTypes().addAll(inject(EvaluationTypeDao.class).read(SchoolConstant.Code.EvaluationType.COLLECTION));
    	
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.PK,null,1l}}, Boolean.TRUE, Boolean.FALSE);
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{{SchoolConstant.Code.LevelName.G1,"A",1l}}, Boolean.TRUE, Boolean.FALSE);
    	
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
