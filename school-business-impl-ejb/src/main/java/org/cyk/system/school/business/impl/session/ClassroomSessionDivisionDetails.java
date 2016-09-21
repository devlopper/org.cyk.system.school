package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionDetails extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @IncludeInputs private ClassroomSessionDetails classroomSessionDetails;
	@Input @InputText private String classroomSession,coefficient,index;
	
	public ClassroomSessionDivisionDetails(ClassroomSessionDivision classroomSessionDivision) {
		super(classroomSessionDivision);
		classroomSessionDetails = new ClassroomSessionDetails(classroomSessionDivision.getClassroomSession());
		classroomSession = formatUsingBusiness(classroomSessionDivision);
		coefficient = formatNumber(classroomSessionDivision.getCoefficient());
		index = formatNumber(classroomSessionDivision.getIndex());
		/*fromDate = timeBusiness.formatDate(classroomSessionDivision.getExistencePeriod().getFromDate());
		toDate = timeBusiness.formatDate(classroomSessionDivision.getExistencePeriod().getToDate());
		duration = numberBusiness.format(inject(ClassroomSessionBusiness.class)
				.convertAttendanceTimeToDivisionDuration(classroomSessionDivision.getClassroomSession(),classroomSessionDivision.getNumberOfMillisecond()));
		*/
	}
	
	public static final String FIELD_CLASSROOM_SESSION_DETAILS = "classroomSessionDetails";
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	public static final String FIELD_COEFFICIENT = "coefficient";
	public static final String FIELD_INDEX = "index";
}