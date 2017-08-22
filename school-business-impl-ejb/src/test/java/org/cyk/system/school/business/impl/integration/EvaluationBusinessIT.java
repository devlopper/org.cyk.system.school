package org.cyk.system.school.business.impl.integration;

import java.math.BigDecimal;

import org.cyk.system.root.business.impl.AbstractBusinessTestHelper.TestCase;
import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.impl._dataproducer.EnterpriseResourcePlanningFakedDataProducer;
import org.cyk.system.school.business.impl.integration.enterpriseresourceplanning.AbstractEnterpriseResourcePlanningBusinessIT;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.persistence.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeDao;
import org.cyk.utility.common.generator.RandomDataProvider;
import org.junit.Test;

public class EvaluationBusinessIT extends AbstractEnterpriseResourcePlanningBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Test
    public void crud(){
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = inject(ClassroomSessionDivisionSubjectEvaluationTypeDao.class).readOneRandomly();
    	Evaluation evaluation = inject(EvaluationBusiness.class).instanciateOne(classroomSessionDivisionSubjectEvaluationType).setCode("e001");
    	for(StudentClassroomSessionDivisionSubjectEvaluation studentClassroomSessionDivisionSubjectEvaluation 
    			: evaluation.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection())
    		studentClassroomSessionDivisionSubjectEvaluation.setValue(new BigDecimal(RandomDataProvider.getInstance().randomInt(0, 100)));
    	
    	TestCase testCase = instanciateTestCase();
    	testCase.create(evaluation);
    	evaluation = testCase.read(Evaluation.class, "e001");
    	evaluation.setName("MyEval");
    	testCase.update(evaluation);
    	evaluation = testCase.read(Evaluation.class, "e001");
    	assertEquals("MyEval", evaluation.getName());
    	testCase.delete(evaluation);
    	testCase.clean();
    }
    
    @Test
    public void average(){
    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    	//04/10/200018/06/2001G1YEAR1ATRIMESTER1HANDWRITINGTEST2
    	//STUDG1A04/10/200018/06/2001G1YEAR1ATRIMESTER2MORAL_EDUCATION
    	Evaluation evaluation = inject(EvaluationBusiness.class).instanciateOne("04/10/200018/06/2001G1YEAR1ATRIMESTER1MORAL_EDUCATIONTEST1",new String[][]{
    			{"STUDG1A04/10/200018/06/2001G1YEAR1ATRIMESTER1MORAL_EDUCATION","75"}
    	}).setCode("e001");
    	
    	TestCase testCase = instanciateTestCase();
    	testCase.create(evaluation);
    	evaluation = testCase.read(Evaluation.class, "e001");
    	evaluation.setName("MyEval");
    	testCase.update(evaluation);
    	evaluation = testCase.read(Evaluation.class, "e001");
    	assertEquals("MyEval", evaluation.getName());
    	testCase.delete(evaluation);
    	testCase.clean();
    }
    
    /*@Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	EnterpriseResourcePlanningFakedDataProducer dataProducer = (EnterpriseResourcePlanningFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
  
    	dataProducer.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	return dataProducer;
    }*/

}
