package org.cyk.system.school.business.impl.integration.enterpriseresourceplanning;

import javax.inject.Inject;

import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;

public abstract class AbstractEnterpriseResourcePlanningBusinessIT extends AbstractBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected EnterpriseResourcePlanningFakedDataProducer dataProducer;
    
    protected void installApplication(Boolean fake){
    	super.installApplication(fake);
    	CompanyBusinessLayer.getInstance().enableEnterpriseResourcePlanning();
    	SchoolBusinessLayer.getInstance().enableEnterpriseResourcePlanning();
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return dataProducer;
    }
    
}
