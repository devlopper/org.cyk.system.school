package org.cyk.system.school.business.impl.iesa;

public class PopulateWithFakedDataBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.TRUE);
    	dataProducer.setNumbreOfTeachers(12 * 5);
    	dataProducer.setNumbreOfStudents(12 * 30 * 5);
    	installApplication();
    	System.exit(0);
    }
    
}
