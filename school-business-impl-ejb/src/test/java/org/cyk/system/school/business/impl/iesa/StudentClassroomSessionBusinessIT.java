package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.impl.SchoolDataProducerHelper.ClassroomSessionInfos;
import org.cyk.system.school.model.actor.Student;

public class StudentClassroomSessionBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	schoolBusinessTestHelper.getEvaluationTypes().addAll(dataProducer.getEvaluationTypes());
    	
    	schoolBusinessTestHelper.createActors(Student.class,new String[]{"STUD1"/*,"STUD2","STUD3","STUD4","STUD5"*/});
    	ClassroomSessionInfos grade = dataProducer.getG1();
    	/*StudentClassroomSession studentClassroomSession = */schoolBusinessTestHelper.createStudentClassroomSession("STUD1", grade.getClassroomSession()
    			,new Object[][]{ {15},{15},{15} });
    	
    	//StudentClassroomSessionDivision studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionBusiness.class)
    	//		.findByStudentByClassroomSessionDivision(inject(StudentBusiness.class).find("STUD1"), grade.division(0).getClassroomSessionDivision());
    	/*
    	schoolBusinessTestHelper.createStudentClassroomSessionDivisionReport(studentClassroomSessionDivision.getClassroomSessionDivision(), Boolean.FALSE);
    	studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionBusiness.class).find(studentClassroomSessionDivision.getIdentifier());
    	Assert.assertNull("Report is null", studentClassroomSessionDivision.getResults().getReport());
    	
    	schoolBusinessTestHelper.updateStudentClassroomSessionDivision(studentClassroomSessionDivision,
    			inject(StudentResultsMetricValueBusiness.class).findByStudentResults(studentClassroomSessionDivision.getResults()),new String[]{
    		"1","2","3","4","5","6","7","8","9","10","11","12"	
    	});
    	
    	schoolBusinessTestHelper.updateStudentClassroomSessionDivision(studentClassroomSessionDivision,
    			inject(StudentResultsMetricValueBusiness.class).findByStudentResults(studentClassroomSessionDivision.getResults()),new String[]{
    		"3","5","4","4","1","2","7","1","1","2","4","3"	
    	});
    	
    	schoolBusinessTestHelper.createStudentClassroomSessionDivisionReport(studentClassroomSessionDivision.getClassroomSessionDivision(), Boolean.FALSE);
    	studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionBusiness.class).find(studentClassroomSessionDivision.getIdentifier());
    	Assert.assertNull("Report is null", studentClassroomSessionDivision.getResults().getReport());
    	
    	ClassroomSessionDivisionSubject classroomSessionDivisionSubject = grade.subject(0, 0);
    	schoolBusinessTestHelper.createSubjectEvaluations(classroomSessionDivisionSubject,new String[][]{
    		{"STUD1","90","30","60"}
    	});
    	
    	schoolBusinessTestHelper.createStudentClassroomSessionDivisionReport(studentClassroomSessionDivision.getClassroomSessionDivision(), Boolean.TRUE);
    	studentClassroomSessionDivision = inject(StudentClassroomSessionDivisionBusiness.class).find(studentClassroomSessionDivision.getIdentifier());
    	Assert.assertNotNull("Report is not null", studentClassroomSessionDivision.getResults().getReport());
    	*/
    }
    
    

}
