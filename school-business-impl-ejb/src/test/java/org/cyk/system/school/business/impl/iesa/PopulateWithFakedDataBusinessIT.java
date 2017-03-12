package org.cyk.system.school.business.impl.iesa;


public class PopulateWithFakedDataBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	installApplication();
    	System.exit(0);
    }
    
}
