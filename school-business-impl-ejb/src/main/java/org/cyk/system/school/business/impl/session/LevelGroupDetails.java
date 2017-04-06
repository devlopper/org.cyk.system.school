package org.cyk.system.school.business.impl.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Getter @Setter
public class LevelGroupDetails extends AbstractOutputDetails<LevelGroup> implements Serializable{
	private static final long serialVersionUID = -4741435164709063863L;
	
	@Input @InputText private FieldValue studentClassroomSessionDivisionResultsReportSigner;
	
	public LevelGroupDetails(LevelGroup levelGroup) {
		super(levelGroup);
		studentClassroomSessionDivisionResultsReportSigner = new FieldValue(levelGroup.getNodeInformations().getStudentClassroomSessionDivisionResultsReportSigner());
	}
	
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_SIGNER = "studentClassroomSessionDivisionResultsReportSigner";
}