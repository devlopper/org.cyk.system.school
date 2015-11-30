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
    	parameters.setTeacherCount(3);
    	parameters.setStudentCount(3);
    	parameters.setGeneratedStudentInClassroomSessionCount(1);
    	parameters.setStudentByClassroomSessionCount(1);
    	schoolBusinessTestHelper.simulate(parameters);
    }
    
}
