package org.cyk.system.school.business.impl.iesa;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;

public class StudentClassroomSessionDivisionReportBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
        
    @Override
    protected void businesses() {
    	schoolBusinessTestHelper.generateStudentClassroomSessionDivisionReport( ((IesaFakedDataProducer)getFakedDataProducer()).generate()
    			, new Boolean[]{Boolean.FALSE},Boolean.TRUE, Boolean.FALSE);
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G8_YEAR_1);
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	dataProducer.getDivisionOrderNumbers().add(2l);
    	dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
