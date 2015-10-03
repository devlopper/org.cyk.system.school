package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.utility.test.Transaction;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public class ReportCardBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Deployment
    public static Archive<?> createDeployment() {
        return createRootDeployment();
    }
    
    @Inject private SchoolBusinessTestHelper schoolBusinessTestHelper;
    @Inject private IesaFakedDataProducer dataProducer;
     
    @Inject private UserTransaction userTransaction;
    
    @Override
    protected void businesses() {
    	installApplication();
    	new Transaction(this,userTransaction,null){
			@Override
			public void _execute_() {
				dataProducer.produce();
			}
    	}.run();
    	schoolBusinessTestHelper.setCoefficientApplied(Boolean.FALSE);
    	
    	
    	schoolBusinessTestHelper.registerStudents(new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	
    	schoolBusinessTestHelper.takeSubjects(new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"}
    	, new ClassroomSessionDivisionSubject[]{dataProducer.getSubjectEnglishLanguage(),dataProducer.getSubjectFrench(),dataProducer.getSubjectMathematics()
    			,dataProducer.getSubjectPhysics(),dataProducer.getSubjectChemistry()}); 
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectAfterEvaluation(dataProducer.getSubjectEnglishLanguage(), dataProducer.getEvaluationTypeNameTest1(), new String[][]{
    		{"STUD1","60","60","2"},{"STUD2","90","90","1"},{"STUD3","40","40","4"},{"STUD4","45","45","3"},{"STUD5","20","20","5"}
    	});
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectAfterEvaluation(dataProducer.getSubjectEnglishLanguage(), dataProducer.getEvaluationTypeNameTest2(), new String[][]{
    		{"STUD1","50","55","3"},{"STUD2","30","60","1"},{"STUD3","60","50","4"},{"STUD4","45","45","5"},{"STUD5","95","57.5","2"}
    	});
    }
    
    

}
