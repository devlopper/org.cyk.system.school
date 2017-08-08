package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.utility.common.annotation.user.interfaces.IncludeInputs;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

public class ClassroomSessionDetails extends AbstractOutputDetails<ClassroomSession> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private FieldValue academicSession,levelTimeDivision,suffix,coordinator;
	@Input @InputText private String numberOfStudent;
	@IncludeInputs private CommonNodeInformationsDetails commonNodeInformations = new CommonNodeInformationsDetails(null);
	
	public ClassroomSessionDetails(ClassroomSession classroomSession) {
		super(classroomSession);	
	}
	
	@Override
	public void setMaster(ClassroomSession classroomSession) {
		super.setMaster(classroomSession);
		if(classroomSession!=null){
			academicSession = new FieldValue(classroomSession.getAcademicSession());
			levelTimeDivision = new FieldValue(classroomSession.getLevelTimeDivision());
			suffix = new FieldValue(classroomSession.getSuffix());
			coordinator = new FieldValue(classroomSession.getCoordinator());
			numberOfStudent=formatNumber(classroomSession.getResults().getNumberOfStudent());
			if(commonNodeInformations==null)
				commonNodeInformations = new CommonNodeInformationsDetails(classroomSession.getNodeInformations());
			else
				commonNodeInformations.setMaster(classroomSession.getNodeInformations());
		}
	}
	
	public static final String FIELD_ACADEMIC_SESSION = "academicSession";
	public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
	public static final String FIELD_SUFFIX = "suffix";
	public static final String FIELD_COORDINATOR = "coordinator";
	public static final String FIELD_COMMON_NODE_INFORMATIONS = "commonNodeInformationsDetails";
	public static final String FIELD_NUMBER_OF_STUDENT = "numberOfStudent";
}