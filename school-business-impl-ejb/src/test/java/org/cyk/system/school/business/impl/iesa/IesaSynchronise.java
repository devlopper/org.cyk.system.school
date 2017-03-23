package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;

public class IesaSynchronise extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
            
    @Override
    protected void businesses() {
    	System.exit(0);
    }
    
    @Override
    protected void installApplication(Boolean fake) {}
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return super.getFakedDataProducer().setStructurationEnabled(Boolean.FALSE).setSynchronizationEnabled(Boolean.TRUE);
    }
}
