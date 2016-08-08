package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionDetails extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String name,fromDate,toDate,duration;
	
	public ClassroomSessionDivisionDetails(ClassroomSessionDivision classroomSessionDivision) {
		super(classroomSessionDivision);
		name = formatUsingBusiness(classroomSessionDivision);
		fromDate = timeBusiness.formatDate(classroomSessionDivision.getExistencePeriod().getFromDate());
		toDate = timeBusiness.formatDate(classroomSessionDivision.getExistencePeriod().getToDate());
		duration = numberBusiness.format(SchoolBusinessLayer.getInstance().getClassroomSessionBusiness()
				.convertAttendanceTimeToDivisionDuration(classroomSessionDivision.getClassroomSession(),classroomSessionDivision.getNumberOfMillisecond()));
	}
}