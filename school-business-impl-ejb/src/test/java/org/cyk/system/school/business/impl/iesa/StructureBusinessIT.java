package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Student;

public class StructureBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	schoolBusinessTestHelper.createActors(Student.class,new String[]{"STUD1"/*,"STUD2","STUD3","STUD4","STUD5"*/});
    	
    	
    	assertEquals("No student classroom session division", 0l, SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().countAll());
    	
    	/*StudentClassroomSession studentClassroomSession = */schoolBusinessTestHelper.createStudentClassroomSession("STUD1", dataProducer.getGrade1().getClassroomSession()
    			,new Object[][]{ {15},{15},{15} });
    	
    	assertEquals("Student classroom session count", 1l, SchoolBusinessLayer.getInstance().getStudentClassroomSessionBusiness().countAll());
    	assertEquals("Student classroom session division count", 3l, SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().countAll());
    	assertEquals("Student classroom session division subject count", 45l, SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().countAll());
    	
    	
    	schoolBusinessTestHelper.createStudentSubject("STUD1", dataProducer.getGrade2().subject(0, 0), new Object[][]{ {1} });
    }
    
}
