package org.cyk.system.school.business.impl.integration;

import java.io.File;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.apache.commons.io.FileUtils;
import org.cyk.system.school.business.impl.iesa.IesaFakedDataProducer;
import org.cyk.utility.test.Transaction;

public class GlassfishFileReadBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
     
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    @Inject private UserTransaction userTransaction;
    
    @Override
    protected void businesses() {
    	File userDirectory = new File(System.getProperty("user.dir"));
    	File arquillianFile = new File(userDirectory,"/src/test/resources/arquillian.xml");
    	System.out.println(arquillianFile);
    	updateArquillianResourceXml("");
    }
        
}
