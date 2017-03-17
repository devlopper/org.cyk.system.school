package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.impl._test.SchoolBusinessTestHelper.SchoolBusinessSimulationParameters;
import org.cyk.system.school.model.SchoolConstant;

public class SimulateBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
 
    @Override
    protected void businesses() {
    	installApplication();

    	SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED = Boolean.FALSE;
    
    	SchoolBusinessSimulationParameters parameters = new SchoolBusinessSimulationParameters();

    	parameters.setGeneratedClassroomSessionCountByLevel(null);
    	parameters.getClassroomSessionDivisionIndexes().add(0);
    	
    	schoolBusinessTestHelper.simulate(parameters);
    }
    
}
