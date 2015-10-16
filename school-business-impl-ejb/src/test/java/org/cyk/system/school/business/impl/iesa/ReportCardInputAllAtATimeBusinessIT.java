package org.cyk.system.school.business.impl.iesa;

import java.util.Arrays;

import javax.inject.Inject;
import javax.transaction.UserTransaction;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.utility.test.Transaction;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;

public class ReportCardInputAllAtATimeBusinessIT extends AbstractBusinessIT {

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
    	SchoolBusinessLayer.getInstance().setReportProducer(new IesaFakedDataProducer.ReportProducer());
    	schoolBusinessTestHelper.setCoefficientApplied(Boolean.FALSE);
    	
    	schoolBusinessTestHelper.registerActors(Student.class,new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"});
    	
    	schoolBusinessTestHelper.takeSubjects(new String[]{"STUD1","STUD2","STUD3","STUD4","STUD5"}
    		,dataProducer.getGrade1Subjects().toArray(new ClassroomSessionDivisionSubject[]{})); 
    	
    	schoolBusinessTestHelper.randomMetricValues(Arrays.asList(dataProducer.getClassroomSessionDivision1()));
    	
    	schoolBusinessTestHelper.getEvaluationTypes().add(dataProducer.getEvaluationTypeNameTest1());
    	schoolBusinessTestHelper.getEvaluationTypes().add(dataProducer.getEvaluationTypeNameTest2());
    	schoolBusinessTestHelper.getEvaluationTypes().add(dataProducer.getEvaluationTypeNameExam());
    	
    	schoolBusinessTestHelper.getClassroomSessionDivisionSubjects().add(dataProducer.getSubjectEnglishLanguage());
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionAfterEvaluation( 
    			new String[][]{{"STUD1","60","50","70","65.5","2"}
    			              ,{"STUD2","90","30","60","60","3"}
    			              ,{"STUD3","40","60","40","43","5"}
    			              ,{"STUD4","45","45","80","69.5","1"}
    			              ,{"STUD5","20","95","55","55.75","4"}});
    	
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(Arrays.asList(dataProducer.getClassroomSessionDivision1()), Boolean.TRUE);
    }
    
    
    
}
