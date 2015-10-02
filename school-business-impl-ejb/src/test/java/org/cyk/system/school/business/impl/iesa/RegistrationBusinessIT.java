package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.business.impl.integration.IesaFakedDataProducer;
import org.cyk.utility.test.Transaction;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public class RegistrationBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Deployment
    public static Archive<?> createDeployment() {
        return createRootDeployment();
    }
    
    @Inject private SchoolBusinessTestHelper schoolBusinessTestHelper;
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
     
    @Inject private UserTransaction userTransaction;
    
    @Override
    protected void businesses() {
    	installApplication();
    	new Transaction(this,userTransaction,null){
			@Override
			public void _execute_() {
				iesaFakedDataProducer.produce();
			}
    	}.run();
    	
    	//dataProducer.produce();
    	
    	schoolBusinessTestHelper.registerStudent("STUD1", null);
    	schoolBusinessTestHelper.registerStudent("STUD2", null);
    	schoolBusinessTestHelper.registerStudent("STUD3", null);
    	schoolBusinessTestHelper.registerStudent("STUD4", null);
    	schoolBusinessTestHelper.registerStudent("STUD5", null);
    	
    	
    }
    
    

}
