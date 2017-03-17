package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;

import org.cyk.system.company.business.impl.AbstractCompanyReportProducer;
import org.cyk.system.company.business.impl.CompanyBusinessLayer;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.RootConstant;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.business.impl.actor.StudentBusinessImpl;
import org.cyk.system.school.business.impl.integration.enterpriseresourceplanning.AbstractEnterpriseResourcePlanningBusinessIT;

public abstract class AbstractIesaBusinessIT extends AbstractEnterpriseResourcePlanningBusinessIT {

	private static final long serialVersionUID = -5752455124275831171L;

    @Inject protected IesaFakedDataProducer dataProducer; 
     
    protected void installApplication(Boolean fake){	
    	super.installApplication(fake);
    	
    	StudentBusinessImpl.Listener.Adapter listener = new StudentBusinessImpl.Listener.Adapter.Default.EnterpriseResourcePlanning();
    	listener.setCodePrefix("IESA");
    	StudentBusinessImpl.Listener.COLLECTION.add(listener);
    	
    	CompanyBusinessLayer.getInstance().enableEnterpriseResourcePlanning();
		
    	AbstractCompanyReportProducer.Listener.COLLECTION.add(new AbstractCompanyReportProducer.Listener.Adapter.Default(){
			private static final long serialVersionUID = 215473098986115952L;
			
			@Override
			public String[] getCustomerPersonRelationshipTypeCodes(AbstractIdentifiable identifiable) {
				return new String[]{RootConstant.Code.PersonRelationshipType.FAMILY_FATHER,RootConstant.Code.PersonRelationshipType.FAMILY_MOTHER};
			}
			
			@Override
			public String getCustomerLabel(AbstractIdentifiable identifiable) {
				return "Parent";
			}
		});
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	return dataProducer;
    }
    
}
