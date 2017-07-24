package org.cyk.system.school.business.impl.integration;

import org.cyk.system.school.business.impl.iesa.AbstractIesaBusinessIT;

public class GetParameterIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void installApplication() {}
    
    @Override
    protected void businesses() {
    	System.out.println(System.getProperty("param1"));
    	System.out.println(System.getProperty("param2"));
    }
        
}
