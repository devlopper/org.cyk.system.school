package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.cyk.system.school.business.api.session.SchoolReportProducer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionDivisionInfos;
import org.cyk.system.school.model.actor.Student;

public class ReportCardInputAllAtATimeBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.getEvaluationTypeCodes().addAll(Arrays.asList("Test1","Test2","Exam"));
    	SchoolReportProducer.DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS.setSumMarks(Boolean.TRUE);
    	schoolBusinessTestHelper.registerActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().addAll(dataProducer.getGrade1().division(0).getClassroomSessionDivisionSubjects());
    	schoolBusinessTestHelper.createStudentClassroomSessionDivisionSubjects(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    		dataProducer.getGrade1().division(0).getClassroomSessionDivisionSubjects()); 
    	
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().clear();
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(dataProducer.getGrade1().division(0).subject(0).getClassroomSessionDivisionSubject());
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(dataProducer.getEvaluationTypes());
    	
    	//trimesterEverybodyHaveAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    	
    	trimesterEverybodyHaveNotAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    }
    
    private void trimesterEverybodyHaveAllEvaluations(ClassroomSessionDivisionInfos classroomSessionDivisionInfos,Boolean generateReport,Boolean printReport){
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().clear();
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject());
    	/*schoolBusinessTestHelper.assertClassroomSessionDivisionAfterEvaluation( 
    			new String[][]{{"STUD1","60","50","70","65.5",/*"2""1"}
    			              /*,{"STUD2","90","30","60","60","3"}
    			              ,{"STUD3","40","60","40","43","5"}
    			              ,{"STUD4","45","45","80","69.5","1"}
    			              ,{"STUD5","20","95","55","55.75","4"}
    			              
    			});*/
    	
    	if(Boolean.TRUE.equals(generateReport)){
    		schoolBusinessTestHelper.randomMetricValues(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()));
    		//schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()),printReport);
    	}
    }
    
    private void trimesterEverybodyHaveNotAllEvaluations(ClassroomSessionDivisionInfos classroomSessionDivisionInfos,Boolean generateReport,Boolean printReport){
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    		{"STUD1","90","30","60"}
    		,{"STUD2","70","50","60"}
              /*,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});
    	
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(1).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1",null,"50","70"}
    		,{"STUD2","90","15","65"}
              /*,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});
    	
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(2).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1",null,null,"70"}
    		,{"STUD2","45","50","50"}
              /*,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});
    	
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(3).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1",null,null,null}
    		,{"STUD2","80","30","75"}
              /*,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});
    	
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(4).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1","50",null,"70"}
    		,{"STUD2","55","75","60"}
             /* ,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});
    	
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(5).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1","50","70",null}
    		,{"STUD2","80","80","40"}
              /*,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});
    	
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionInfos.subject(6).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1",null,null,"70"}
    		,{"STUD2","50","50","50"}
              /*,{"STUD3","40","60","40"}
              ,{"STUD4","45","45","80"}
              ,{"STUD5","20","95","55"}*/
    	});

    	 
    	/*
    	schoolBusinessTestHelper.asserts(classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject(),new String[][]{{
    		"STUD1","66.47","1"}
    		/*,{"STUD2","",""}
              ,{"STUD3","",""}
              ,{"STUD4","",""}
              ,{"STUD5","",""}
    	});*/
    	
    	/*schoolBusinessTestHelper.assertClassroomSessionDivisionAfterEvaluation( 
    			new String[][]{{"STUD1",null,"50","70","66.47","1"}
    			              ,{"STUD2","90","30","60","60","3"}
    			              ,{"STUD3","40","60","40","43","5"}
    			              ,{"STUD4","45","45","80","69.5","1"}
    			              ,{"STUD5","20","95","55","55.75","4"}
    			              
    			});*/
    	
    	if(Boolean.TRUE.equals(generateReport)){
    		schoolBusinessTestHelper.randomMetricValues(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()));
    		schoolBusinessTestHelper.createStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()),printReport);
    	}
    }
    
}
