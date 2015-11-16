package org.cyk.system.school.business.impl.iesa;

import java.math.BigDecimal;
import java.util.Collection;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.utility.common.generator.RandomDataProvider;

public class StudentClassroomSessionBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	schoolBusinessTestHelper.registerActors(Student.class,new String[]{"STUD1"/*,"STUD2","STUD3","STUD4","STUD5"*/});
    	StudentClassroomSession studentClassroomSession = schoolBusinessTestHelper.createStudentClassroomSession("STUD1", dataProducer.getGrade1().getClassroomSession()
    			,new Object[][]{ {15},{15},{15} });
    	
    	
    	StudentClassroomSessionDivision studentClassroomSessionDivision = SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findOneRandomly();
    	
    	schoolBusinessTestHelper.updateStudentClassroomSessionDivision(studentClassroomSessionDivision,
    			SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResults(studentClassroomSessionDivision.getResults()),new String[]{
    		"1","2","3","4","5","6","7","8","9","10","11","12"	
    	});
    	
    	schoolBusinessTestHelper.updateStudentClassroomSessionDivision(studentClassroomSessionDivision,
    			SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResults(studentClassroomSessionDivision.getResults()),new String[]{
    		"3","5","4","4","1","2","7","1","1","2","4","3"	
    	});
    }
    
    

}
