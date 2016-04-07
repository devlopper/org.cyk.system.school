package org.cyk.system.school.business.impl.iesa;


public class PopulateWithFakedDataBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.TRUE);
    	dataProducer.setGenerateStudentClassroomSessionDivisionReport(Boolean.FALSE);
    	dataProducer.setNumbreOfLecturesByClassroomSessionDivisionSubject(0);
    	dataProducer.setNumbreOfTeachers(12/* * 5*/);
    	dataProducer.setNumbreOfStudents(12 /* 30 * 5*/);
    	dataProducer.setNumbreOfStudentsByClassroomSession(3);
    	installApplication();
    	System.exit(0);
    }
    
}
