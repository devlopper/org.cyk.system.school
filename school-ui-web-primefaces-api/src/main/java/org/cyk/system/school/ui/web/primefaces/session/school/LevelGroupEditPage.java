package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.ui.api.model.AbstractBusinessIdentifiedEditFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;

@Named @ViewScoped @Getter @Setter
public class LevelGroupEditPage extends AbstractCrudOnePage<LevelGroup> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
			
	public static class Form extends AbstractBusinessIdentifiedEditFormModel<LevelGroup> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Person studentClassroomSessionDivisionResultsReportSigner;
	
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

}
