package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;

public class IesaApplicationSetupBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
   
    @Override
    protected void businesses() {
    	System.exit(0);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return iesaFakedDataProducer;
    }
        
}
