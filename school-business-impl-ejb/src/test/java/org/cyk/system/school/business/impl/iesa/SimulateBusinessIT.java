package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.SchoolBusinessSimulationParameters;
import org.cyk.system.school.model.SchoolConstant;

public class SimulateBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();

    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    
    	SchoolBusinessSimulationParameters parameters = new SchoolBusinessSimulationParameters();

    	parameters.setGeneratedClassroomSessionCountByLevel(null);
    	parameters.getClassroomSessionDivisionIndexes().add(0);
    	
    	schoolBusinessTestHelper.setCustomClassroomSessionDivisionSubjectEvaluationTypeInfos(new Object[][]{
    		{dataProducer.getEvaluationTypeTest1(),"0.15","100"},{dataProducer.getEvaluationTypeTest2(),"0.15","100"},{dataProducer.getEvaluationTypeExam(),"0.7","100"}
    	});
    	
    	schoolBusinessTestHelper.simulate(parameters);
    }
    
}
