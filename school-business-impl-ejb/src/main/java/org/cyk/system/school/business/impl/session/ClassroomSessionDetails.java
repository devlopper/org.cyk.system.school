package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDetails extends AbstractOutputDetails<ClassroomSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String academicSession,levelTimeDivision,suffix,coordinator,numberOfStudent;
	
	public ClassroomSessionDetails(ClassroomSession classroomSession) {
		super(classroomSession);
		academicSession = formatUsingBusiness(classroomSession.getAcademicSession());
		levelTimeDivision = formatUsingBusiness(classroomSession.getLevelTimeDivision());
		suffix = formatUsingBusiness(classroomSession.getSuffix());
		coordinator = classroomSession.getCoordinator()==null?Constant.EMPTY_STRING:classroomSession.getCoordinator().getPerson().getNames();
		numberOfStudent=formatNumber(classroomSession.getResults().getNumberOfStudent());
	}
	
	public static final String FIELD_ACADEMIC_SESSION = "academicSession";
	public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
	public static final String FIELD_SUFFIX = "suffix";
	public static final String FIELD_COORDINATOR = "coordinator";
	public static final String FIELD_NUMBER_OF_STUDENT = "numberOfStudent";
}