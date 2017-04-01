package org.cyk.system.school.business.impl.iesa;

import java.util.Collection;

import org.cyk.system.root.business.impl.AbstractFakedDataProducer;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl._dataproducer.IesaFakedDataProducer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDao;
import org.cyk.system.school.persistence.api.session.StudentClassroomSessionDivisionDao;

public class IesaStudentClassroomSessionCrudBusinessIT extends AbstractIesaBusinessIT {

    private static final long serialVersionUID = -6691092648665798471L;
        
    @Override
    protected void businesses() {
    	Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = inject(StudentClassroomSessionDivisionDao.class).readAll();
    	System.out.println("#StudentClassroomSessionDivision : "+studentClassroomSessionDivisions.size());
    	for(StudentClassroomSessionDivision studentClassroomSessionDivision : studentClassroomSessionDivisions){
    		inject(StudentClassroomSessionDivisionBusiness.class).delete(studentClassroomSessionDivision);
    		System.out.println("Deleted : "+studentClassroomSessionDivision);
    	}
    }
    
    @Override
    protected AbstractFakedDataProducer getFakedDataProducer() {
    	IesaFakedDataProducer dataProducer = (IesaFakedDataProducer) super.getFakedDataProducer().setDoBusiness(Boolean.TRUE);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().clear();
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.PK_YEAR_1);
    	dataProducer.getClassroomSessionLevelTimeDivisionCodes().add(SchoolConstant.Code.LevelTimeDivision.G1_YEAR_1);
    	
    	dataProducer.getDivisionOrderNumbers().clear();
    	dataProducer.getDivisionOrderNumbers().add(1l);
    	//dataProducer.getDivisionOrderNumbers().add(2l);
    	//dataProducer.getDivisionOrderNumbers().add(3l);
    	return dataProducer;
    }
        
}
