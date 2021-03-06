package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputChoiceAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneAutoComplete;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionEditPage extends AbstractCrudOnePage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected StudentClassroomSession instanciateIdentifiable() {
		StudentClassroomSession studentClassroomSession = super.instanciateIdentifiable();
		if(studentClassroomSession.getClassroomSession()==null)
			studentClassroomSession.setClassroomSession(webManager.getIdentifiableFromRequestParameter(ClassroomSession.class, Boolean.TRUE));
		return studentClassroomSession;
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_CLASSROOM_SESSION, inject(ClassroomSessionBusiness.class).findByAcademicSession(
				inject(AcademicSessionBusiness.class).findCurrent(null)),identifiable.getClassroomSession());
	}
		
	public static class Form extends AbstractFormModel<StudentClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputChoiceAutoComplete @InputOneChoice @InputOneAutoComplete private Student student;
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		
		public static final String FIELD_CLASSROOM_STUDENT = "student";
		public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	}
	
}
