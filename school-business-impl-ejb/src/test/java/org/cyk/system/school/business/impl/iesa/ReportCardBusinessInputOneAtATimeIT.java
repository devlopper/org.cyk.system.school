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

public class ReportCardBusinessInputOneAtATimeIT extends AbstractBusinessIT {

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
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectAfterEvaluation(dataProducer.getSubjectEnglishLanguage(), dataProducer.getEvaluationTypeNameTest1(), 
    			new String[][]{{"STUD1","60","60","2"},{"STUD2","90","90","1"},{"STUD3","40","40","4"},{"STUD4","45","45","3"},{"STUD5","20","20","5"}});
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectAfterEvaluation(dataProducer.getSubjectEnglishLanguage(), dataProducer.getEvaluationTypeNameTest2(), 
    			new String[][]{{"STUD1","50","55","3"},{"STUD2","30","60","1"},{"STUD3","60","50","4"},{"STUD4","45","45","5"},{"STUD5","95","57.5","2"}});
    	
    	schoolBusinessTestHelper.assertClassroomSessionDivisionSubjectAfterEvaluation(dataProducer.getSubjectEnglishLanguage(), dataProducer.getEvaluationTypeNameExam(), 
    			new String[][]{{"STUD1","70","65.5","2"},{"STUD2","60","60","3"},{"STUD3","40","43","5"},{"STUD4","80","69.5","1"},{"STUD5","55","55.75","4"}});
    	    	
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport(Arrays.asList(dataProducer.getClassroomSessionDivision1()), Boolean.TRUE);
    }
    
    
    
}
