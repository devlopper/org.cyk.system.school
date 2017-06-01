package org.cyk.system.school.ui.web.primefaces.session;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.system.school.model.session.LevelGroupType;
import org.cyk.ui.api.model.pattern.tree.AbstractDataTreeForm;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Getter @Setter 
@FieldOverrides(value = {
		@FieldOverride(name=AbstractDataTreeForm.FIELD_PARENT,type=LevelGroup.class)
		,@FieldOverride(name=AbstractDataTreeForm.FIELD_TYPE,type=LevelGroupType.class)
		}) @Deprecated
public class LevelGroupForm extends AbstractDataTreeForm<LevelGroup,LevelGroupType> {

	private static final long serialVersionUID = -3927257570208213271L;

	@Input @InputChoice @InputOneChoice 
	//@InputChoiceAutoComplete @InputOneAutoComplete 
	@InputOneCombo
	private Person studentClassroomSessionDivisionResultsReportSigner;
	
	@Override
	public void read() {
		super.read();
		studentClassroomSessionDivisionResultsReportSigner = identifiable.getNodeInformations().getStudentClassroomSessionDivisionResultsReportSigner();
	}
	
	@Override
	public void write() {
		super.write();
		identifiable.getNodeInformations().setStudentClassroomSessionDivisionResultsReportSigner(studentClassroomSessionDivisionResultsReportSigner);
	}
	
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_RESULTS_REPORT_SIGNER = "studentClassroomSessionDivisionResultsReportSigner";
}
