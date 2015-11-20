package org.cyk.system.school.business.impl.iesa;


public class SimulateBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();

    	schoolBusinessTestHelper.simulate(3, 3, 1,1, Boolean.FALSE,Boolean.FALSE);
    }
    
}
