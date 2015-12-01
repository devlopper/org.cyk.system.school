package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessTestHelper.SchoolBusinessSimulationParameters;

public class SimulateBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	dataProducer.setGenerateCompleteAcademicSession(Boolean.FALSE);
    	dataProducer.setNumbreOfStudents(0);
    	installApplication();

    	schoolBusinessTestHelper.setCoefficientApplied(Boolean.FALSE);
    	StudentClassroomSessionDivisionBusiness.DEFAULT_BUILD_REPORT_OPTIONS.setAttendance(Boolean.FALSE);
    	
    	SchoolBusinessSimulationParameters parameters = new SchoolBusinessSimulationParameters();

    	parameters.setGeneratedClassroomSessionCountByLevel(null);
    	parameters.getClassroomSessionDivisionIndexes().add(0);
    	
    	schoolBusinessTestHelper.setCustomClassroomSessionDivisionSubjectEvaluationTypeInfos(new Object[][]{
    		{dataProducer.getEvaluationTypeTest1(),"0.15","100"},{dataProducer.getEvaluationTypeTest2(),"0.15","100"},{dataProducer.getEvaluationTypeExam(),"0.7","100"}
    	});
    	
    	schoolBusinessTestHelper.simulate(parameters);
    }
    
}
