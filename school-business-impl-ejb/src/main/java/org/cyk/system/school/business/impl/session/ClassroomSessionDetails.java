package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDetails extends AbstractOutputDetails<ClassroomSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private String academicSession,levelTimeDivision,name,coordinator,numberOfStudents;
	
	public ClassroomSessionDetails(ClassroomSession classroomSession) {
		super(classroomSession);
		academicSession = formatUsingBusiness(classroomSession.getAcademicSession());
		levelTimeDivision = formatUsingBusiness(classroomSession.getLevelTimeDivision());
		name = formatUsingBusiness(classroomSession);
		coordinator = classroomSession.getCoordinator()==null?"":classroomSession.getCoordinator().getPerson().getNames();
		numberOfStudents=formatNumber(classroomSession.getNumberOfStudents());
	}
	
	public static final String FIELD_ACADEMIC_SESSION = "academicSession";
	public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
	public static final String FIELD_SUFFIX = "suffix";
	public static final String FIELD_COORDINATOR = "coordinator";
	public static final String FIELD_NUMBER_OF_STUDENTS = "numberOfStudents";
}