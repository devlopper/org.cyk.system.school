package org.cyk.system.school.business.impl.iesa;

import java.util.List;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.root.business.impl.RootTestHelper;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.test.Transaction;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public class RegistrationBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Deployment
    public static Archive<?> createDeployment() {
        return createRootDeployment();
    }
    
    @Inject private IesaFakedDataProducer iesaFakedDataProducer;
    @Inject private RootTestHelper rootTestHelper;
     
    @Inject private UserTransaction userTransaction;
    
    @Override
    protected void businesses() {
    	installApplication();
    	new Transaction(this,userTransaction,null){
			@Override
			public void _execute_() {
				iesaFakedDataProducer.produce(null);
			}
    	}.run();
    	
    	List<Student> students = rootTestHelper.registerActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	rootTestHelper.assertActorRegistrationCode(Student.class,students, new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    }
    
    

}
