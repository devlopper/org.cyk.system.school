package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDivisionDetails extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@IncludeInputs private ClassroomSessionDetails classroomSessionDetails;
	@Input @InputText private String classroomSession,timeDivisionType,numberOfSubjects;
	
	public ClassroomSessionDivisionDetails(ClassroomSessionDivision classroomSessionDivision) {
		super(classroomSessionDivision);
		classroomSessionDetails = new ClassroomSessionDetails(classroomSessionDivision.getClassroomSession());
		classroomSession = formatUsingBusiness(classroomSessionDivision.getClassroomSession());
		timeDivisionType = formatUsingBusiness(classroomSessionDivision.getTimeDivisionType());
		numberOfSubjects = formatNumber(classroomSessionDivision.getNumberOfSubjects());
		existencePeriod.setDuration(formatNumber(classroomSessionDivision.getExistencePeriod().getNumberOfMillisecond().get()));
	}
	
	public static final String FIELD_CLASSROOM_SESSION_DETAILS = "classroomSessionDetails";
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
	public static final String FIELD_NUMBER_OF_SUBJECTS = "numberOfSubjects";
	
}