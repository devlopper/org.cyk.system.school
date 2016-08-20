package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionEditPage extends AbstractCrudOnePage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	@Override
	protected StudentClassroomSession instanciateIdentifiable() {
		StudentClassroomSession studentClassroomSession = super.instanciateIdentifiable();
		if(studentClassroomSession.getClassroomSession()==null){
			Long classroomSessionIdentifier = requestParameterLong(ClassroomSession.class);
			if(classroomSessionIdentifier==null)
				;
			else{
				studentClassroomSession.setClassroomSession(inject(ClassroomSessionBusiness.class).find(classroomSessionIdentifier));
				
			}
		}
		return studentClassroomSession;
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.CLASSROOM_SESSION, inject(ClassroomSessionBusiness.class).findByAcademicSession(
				inject(AcademicSessionBusiness.class).findCurrent(null)));
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<StudentClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputChoice @InputOneChoice @InputOneCombo private Student student;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		
		public static final String CLASSROOM_SESSION = "classroomSession";
	}
	
}
