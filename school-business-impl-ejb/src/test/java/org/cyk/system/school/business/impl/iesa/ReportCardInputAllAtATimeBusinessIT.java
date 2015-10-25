package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

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
    	schoolBusinessTestHelper.takeSubjects(new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"}); 
    	
    	schoolBusinessTestHelper.randomMetricValues(Arrays.asList(dataProducer.getGrade1().division(0).getClassroomSessionDivision()));
    	
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(dataProducer.getEvaluationTypes());
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(dataProducer.getGrade1().subject(0,0));
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionAfterEvaluation( 
    			new String[][]{{"STUD1","60","50","70","65.5","2"}
    			              ,{"STUD2","90","30","60","60","3"}
    			              ,{"STUD3","40","60","40","43","5"}
    			              ,{"STUD4","45","45","80","69.5","1"}
    			              ,{"STUD5","20","95","55","55.75","4"}});
    	
    	//schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(Arrays.asList(dataProducer.getClassroomSessionDivision1()), Boolean.TRUE);
    }
    
}
