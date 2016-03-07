package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.EvaluationType;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	schoolBusinessTestHelper.createActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getPk().getClassroomSession(), new Object[][]{{0},{0},{0}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getG1().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getG2().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getG3().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getG4().getClassroomSession(), new Object[][]{{dataProducer.getSubjectsG4G6().size()},{dataProducer.getSubjectsG4G6().size()},{dataProducer.getSubjectsG4G6().size()}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getG7().getClassroomSession(), new Object[][]{{dataProducer.getSubjectsG7G9().size()},{dataProducer.getSubjectsG7G9().size()},{dataProducer.getSubjectsG7G9().size()}}); 
    	
    	schoolBusinessTestHelper.createStudentClassroomSessions(new String[]{"STUD1","STUD2"/*,"STUD3","STUD4","STUD5"*/},
    			dataProducer.getG10().getClassroomSession(), new Object[][]{{15},{15},{15}}); 
    	
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(rootDataProducerHelper.getEnumerations(EvaluationType.class));
    	
    	//trimesterEverybodyHaveAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    	
    	//trimesterEverybodyHaveNotAllEvaluations(dataProducer.getGrade1().division(0),Boolean.TRUE,Boolean.TRUE);
    	//trimesterEverybodyHaveNotAllEvaluations(dataProducer.getGrade2().division(0),Boolean.TRUE,Boolean.TRUE);
    	
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getPk().division(0).getClassroomSessionDivision(), null
    			, Boolean.TRUE,Boolean.TRUE);
    	/*
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG1().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG1().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	
    	
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG4().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG4().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG7().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG7().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	
    	schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getG10().division(0).getClassroomSessionDivision(), new Object[][]{
    		new Object[]{dataProducer.getG10().division(0).subject(0).getClassroomSessionDivisionSubject(),new String[][]{
    	    		{"STUD1","90","30","60"}
    	    		//,{"STUD2","70","50","60"}
    	              //,{"STUD3","40","60","40"}
    	              //,{"STUD4","45","45","80"}
    	              //,{"STUD5","20","95","55"}
    	    	}}
    	}, Boolean.TRUE,Boolean.TRUE);
    	*/
    	
    	//schoolBusinessTestHelper.simulateStudentClassroomSessionDivisionReport(dataProducer.getGrade3().division(0).getClassroomSessionDivision(),null, Boolean.TRUE,Boolean.TRUE);
    }
        
}
