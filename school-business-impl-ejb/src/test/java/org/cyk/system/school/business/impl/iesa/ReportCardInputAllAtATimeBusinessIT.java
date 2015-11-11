package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.ClassroomSessionDivisionInfos;
import org.cyk.system.school.model.actor.Student;

public class ReportCardInputAllAtATimeBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	schoolBusinessTestHelper.registerActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().addAll(dataProducer.getGrade1().division(0).getClassroomSessionDivisionSubjects());
    	schoolBusinessTestHelper.takeSubjects(new String[]{"STUD1"/*,"STUD2","STUD3","STUD4","STUD5"*/}); 
    	
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().clear();
    	//schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(dataProducer.getGrade1().division(0).subject(0).getClassroomSessionDivisionSubject());
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(dataProducer.getEvaluationTypes());
    	
    	trimesterEverybodyHaveAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    	
    	//trimesterEverybodyHaveNotAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    }
    
    private void trimesterEverybodyHaveAllEvaluations(ClassroomSessionDivisionInfos classroomSessionDivisionInfos,Boolean generateReport,Boolean printReport){
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().clear();
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject());
    	schoolBusinessTestHelper.assertClassroomSessionDivisionAfterEvaluation( 
    			new String[][]{{"STUD1","60","50","70","65.5",/*"2"*/"1"}
    			              /*,{"STUD2","90","30","60","60","3"}
    			              ,{"STUD3","40","60","40","43","5"}
    			              ,{"STUD4","45","45","80","69.5","1"}
    			              ,{"STUD5","20","95","55","55.75","4"}*/
    			              
    			});
    	
    	if(Boolean.TRUE.equals(generateReport)){
    		schoolBusinessTestHelper.randomMetricValues(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()));
    		schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()),printReport);
    	}
    }
    
    private void trimesterEverybodyHaveNotAllEvaluations(ClassroomSessionDivisionInfos classroomSessionDivisionInfos,Boolean generateReport,Boolean printReport){
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().clear();
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(classroomSessionDivisionInfos.subject(0).getClassroomSessionDivisionSubject());
    	schoolBusinessTestHelper.assertClassroomSessionDivisionAfterEvaluation( 
    			new String[][]{{"STUD1",null,"50","70","66.47","2"}
    			              ,{"STUD2","90","30","60","60","3"}
    			              ,{"STUD3","40","60","40","43","5"}
    			              ,{"STUD4","45","45","80","69.5","1"}
    			              ,{"STUD5","20","95","55","55.75","4"}});
    	
    	if(Boolean.TRUE.equals(generateReport)){
    		schoolBusinessTestHelper.randomMetricValues(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()));
    		schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(Arrays.asList(classroomSessionDivisionInfos.getClassroomSessionDivision()),printReport);
    	}
    }
    
}
