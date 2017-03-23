package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;

public class FakedIesaApplicationSetupBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
        
    @Override
    protected void businesses() {
    	System.exit(0);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.FALSE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K1_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K2_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.K3_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G2_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G3_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G4_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G5_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G6_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G7_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G9_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G10_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G11_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G12_YEAR_1);
    	return dataProducer;
    }
    
}
