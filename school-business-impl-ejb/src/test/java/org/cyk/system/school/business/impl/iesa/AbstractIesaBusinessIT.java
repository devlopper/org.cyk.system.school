package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.utility.test.Transaction;

public abstract class AbstractIesaBusinessIT extends AbstractBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected IesaFakedDataProducer dataProducer;
     
    protected void installApplication(Boolean fake){
    	super.installApplication(fake);
    	new Transaction(this,userTransaction,null){
			@Override
			public void _execute_() {
				dataProducer.produce();
			}
    	}.run();
    	
    	SchoolBusinessLayer.getInstance().setReportProducer(new IesaFakedDataProducer.ReportProducer());
    	schoolBusinessTestHelper.setCoefficientApplied(Boolean.FALSE);
    }
    
}
