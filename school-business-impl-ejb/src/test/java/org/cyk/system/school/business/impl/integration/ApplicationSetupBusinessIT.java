package org.cyk.system.school.business.impl.integration;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.school.business.impl.iesa.IesaFakedDataProducer;
import org.cyk.utility.test.AbstractTest;
import org.cyk.utility.test.Transaction;

public class ApplicationSetupBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    static{
    	updateXmlNode("arquillian.xml","arquillian.xml", AbstractTest.ARQUILLIAN_NAMESPACE, new String[][]{
			new String[]{"container","configuration","property","static value"}
		}); 
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
