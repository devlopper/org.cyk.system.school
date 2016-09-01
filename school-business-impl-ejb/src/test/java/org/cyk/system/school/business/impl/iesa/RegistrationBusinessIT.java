package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.model.session.StudentClassroomSession;

public class RegistrationBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;

    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	//schoolBusinessTestHelper.createActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	StudentClassroomSession studentClassroomSession = schoolBusinessTestHelper.createStudentClassroomSession("STUD1", dataProducer.getG1().getClassroomSession()
    			,new Object[][]{ {15},{15},{15} });
    	schoolBusinessTestHelper.deleteStudentClassroomSession(studentClassroomSession);
    }
    
    

}
