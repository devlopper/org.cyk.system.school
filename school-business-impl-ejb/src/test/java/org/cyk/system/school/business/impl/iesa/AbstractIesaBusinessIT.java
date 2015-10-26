package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;

public abstract class AbstractIesaBusinessIT extends AbstractBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected IesaFakedDataProducer dataProducer;
     
    protected void installApplication(Boolean fake){
    	super.installApplication(fake);
    	SchoolBusinessLayer.getInstance().setReportProducer(new IesaFakedDataProducer.ReportProducer());
    	schoolBusinessTestHelper.setCoefficientApplied(Boolean.FALSE);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return dataProducer;
    }
    
}
