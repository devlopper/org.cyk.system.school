package org.cyk.system.school.business.impl.integration;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.school.business.impl.iesa.IesaFakedDataProducer;
import org.cyk.utility.test.Transaction;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public class ApplicationSetupBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Deployment
    public static Archive<?> createDeployment() {
        return createRootDeployment();
    } 
    
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
    	
    	System.exit(0);
    }
        
}
