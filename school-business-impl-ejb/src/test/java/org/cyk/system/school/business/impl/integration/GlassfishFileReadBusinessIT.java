package org.cyk.system.school.business.impl.integration;

import java.io.File;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.apache.commons.io.FileUtils;
import org.cyk.system.school.business.impl.iesa.IesaFakedDataProducer;
import org.cyk.utility.test.Transaction;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public class GlassfishFileReadBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
     
    static{
    	/*updateXmlNode("arquillian.xml","arquillian.xml", AbstractTest.ARQUILLIAN_NAMESPACE, new String[][]{
			new String[]{"container","configuration","property","static value"}
		}); */
    	System.out.println("GlassfishFileReadBusinessIT.enclosing_method()");
    	updateArquillianResourceXml("src/test/resources-glassfish-embedded/glassfish-resources-live.xml");
    	
    }
    
    @Deployment
    public static Archive<?> createDeployment() {
		System.out.println("AbstractBusinessIT.createDeployment()");
        return createRootDeployment();
    }
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    @Inject private UserTransaction userTransaction;
    
    @Override
    protected void businesses() {
    	System.out.println("GlassfishFileReadBusinessIT.businesses() : "+iesaFakedDataProducer);
    }
        
}
