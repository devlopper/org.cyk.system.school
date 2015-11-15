package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.model.actor.Student;

public class RegistrationBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;

    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	schoolBusinessTestHelper.registerActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	schoolBusinessTestHelper.createStudentClassroomSession("STUD1", dataProducer.getGrade1().getClassroomSession());
    }
    
    

}
