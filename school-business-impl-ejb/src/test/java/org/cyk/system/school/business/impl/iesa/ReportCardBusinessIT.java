package org.cyk.system.school.business.impl.iesa;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.business.impl.integration.IesaFakedDataProducer;
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
    	
    	schoolBusinessTestHelper.evaluateStudents(dataProducer.getSubjectEnglishLanguage(), dataProducer.getEvaluationTypeNameTest1(),new String[][]{
    		{"STUD1","60"},{"STUD2","90"},{"STUD3","40"},{"STUD4","45"},{"STUD5","20"}
    	});
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectAverage(dataProducer.getSubjectEnglishLanguage(), new String[][]{
    		{"STUD1","60"},{"STUD2","90"},{"STUD3","40"},{"STUD4","45"},{"STUD5","20"}
    	});
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectRank(dataProducer.getSubjectEnglishLanguage(), new String[][]{
    		{"STUD1","2","false"},{"STUD2","1","false"},{"STUD3","4","false"},{"STUD4","3","false"},{"STUD5","5","false"}
    	});
    }

}
