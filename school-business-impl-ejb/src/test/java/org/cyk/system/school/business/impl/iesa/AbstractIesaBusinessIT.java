package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.SchoolConstant;

public abstract class AbstractIesaBusinessIT extends AbstractBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected IesaFakedDataProducer dataProducer; 
     
    protected void installApplication(Boolean fake){	
    	super.installApplication(fake);
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	
    	StudentBusinessImpl.Listener.Adapter listener = new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning();
    	listener.setCodePrefix("IESA");
    	StudentBusinessImpl.Listener.COLLECTION.add(listener);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return dataProducer;
    }
    
}
