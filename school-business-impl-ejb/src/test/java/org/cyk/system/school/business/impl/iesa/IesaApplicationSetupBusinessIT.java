package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.school.business.impl.__data__.iesa.FakeDataSet;
import org.cyk.system.school.business.impl.integration.AbstractBusinessIT;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSession;

public class IesaApplicationSetupBusinessIT extends AbstractBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
    
    @Override
    protected void businesses() {
    	FakeDataSet fakedDataSet = new FakeDataSet();
    	fakedDataSet.getIdentifiableCountByTransactionMap().put(ClassroomSession.class, 1);
    	fakedDataSet.getClassroomSessionLevelTimeDivisionCodes().clear();
    	fakedDataSet.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	fakedDataSet.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1);
    	//fakedDataSet.getClassroomSessionSuffixes().clear();
    	//fakedDataSet.getClassroomSessionSuffixes().put(SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1, new String[]{SchoolConstant.Code.ClassroomSessionSuffix.A});
    	fakedDataSet.instanciate();
    	fakedDataSet.create();
    	
    	System.exit(0);
    }
            
}
