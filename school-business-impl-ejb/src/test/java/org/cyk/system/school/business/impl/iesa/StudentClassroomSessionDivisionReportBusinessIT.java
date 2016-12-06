package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.cyk.system.root.business.api.geography.ElectronicMailBusiness;
import org.cyk.system.root.model.party.person.PersonRelationshipType;
import org.cyk.system.school.business.api.actor.StudentBusiness;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.EvaluationType;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	Student student1 = inject(StudentBusiness.class).instanciateOne();
    	student1.setCode("STUD1");
    	student1.setName("komenan");
    	student1.getPerson().setLastnames("yao christian");
    	inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	inject(ElectronicMailBusiness.class).setAddress(student1.getPerson(), PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
    	Student pkStudent = inject(StudentBusiness.class).instanciateOne();
    	pkStudent.setCode("PK_STUD1");
    	pkStudent.setName("Zadi");
    	pkStudent.getPerson().setLastnames("leon");
    	//inject(ElectronicMailBusiness.class).setAddress(pkStudent.getPerson(), PersonRelationshipType.FAMILY_FATHER, "kycdev@gmail.com");
    	//inject(ElectronicMailBusiness.class).setAddress(pkStudent.getPerson(), PersonRelationshipType.FAMILY_MOTHER, "ckydevbackup@gmail.com");
    	
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
    	
    	inject(StudentBusiness.class).create(Arrays.asList(pkStudent,k1Student,k2Student,k3Student,student1));
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{pkStudent.getCode()},dataProducer.getPk().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k1Student.getCode()},dataProducer.getK1().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k2Student.getCode()},dataProducer.getK2().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{k3Student.getCode()},dataProducer.getK3().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{student1.getCode()},
    			dataProducer.getG1().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(rootDataProducerHelper.getEnumerations(EvaluationType.class));
    	
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{
    		{SchoolConstant.Code.LevelName.PK,null,1l}
    		,{SchoolConstant.Code.LevelName.K1,null,1l}
    		,{SchoolConstant.Code.LevelName.K2,null,1l}
    		,{SchoolConstant.Code.LevelName.K3,null,1l}
    		,{SchoolConstant.Code.LevelName.G1,"A",1l}
    	}, Boolean.TRUE, Boolean.FALSE);
    	
    	/*
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(new Object[][]{
    		{SchoolConstant.Code.LevelName.PK,null,1l}
    		,{SchoolConstant.Code.LevelName.PK,null,1l}
    		,{SchoolConstant.Code.LevelName.PK,null,1l}
    		,{SchoolConstant.Code.LevelName.PK,null,1l}
    		,{SchoolConstant.Code.LevelName.PK,null,1l}
    	}, Boolean.FALSE, Boolean.FALSE);
    	*/
    	/*
    	inject(StudentClassroomSessionBusiness.class).updateAverage(Arrays.asList(dataProducer.getG1().getClassroomSession()), new BusinessServiceCallArguments<StudentClassroomSession>());
    	inject(StudentClassroomSessionBusiness.class).updateRank(Arrays.asList(dataProducer.getG1().getClassroomSession()), 
    			schoolBusinessLayer.getStudentEvaluationResultsRankOptions(), new BusinessServiceCallArguments<StudentClassroomSession>());

    	*/
    }
        
}
