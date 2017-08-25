package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.impl.__data__.iesa.RealDataSet;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.session.ClassroomSession;

public class IesaApplicationSetupBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	RealDataSet realDataSet = new RealDataSet();
    	realDataSet.getIdentifiableCountByTransactionMap().put(ClassroomSession.class, 1);
    	realDataSet.instanciate();
    	realDataSet.save();
    	
    	System.exit(0);
    }
            
}
