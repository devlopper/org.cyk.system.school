package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;

public class StructureBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();
    	
    	//schoolBusinessTestHelper.createActors(Student.class,new String[]{"STUD1"/*,"STUD2","STUD3","STUD4","STUD5"*/});
  
    	assertEquals("No student classroom session division", 0l, inject(StudentClassroomSessionBusiness.class).countAll());
    	
    	schoolBusinessTestHelper.createStudentClassroomSession("STUD1", dataProducer.getG1().getClassroomSession(),new Object[][]{ {15},{15},{15} });
    	
    	assertEquals("Student classroom session count", 1l, inject(StudentClassroomSessionBusiness.class).countAll());
    	assertEquals("Student classroom session division count", 3l, inject(StudentClassroomSessionDivisionBusiness.class).countAll());
    	assertEquals("Student classroom session division subject count", 45l, inject(StudentClassroomSessionDivisionSubjectBusiness.class).countAll());
    	
    	schoolBusinessTestHelper.setStudentSubjectCascadeOperationToMaster(Boolean.TRUE);
    	schoolBusinessTestHelper.createStudentSubject("STUD1", dataProducer.getG2().subject(0, 0), new Object[][]{ {1} });
    }
    
}
