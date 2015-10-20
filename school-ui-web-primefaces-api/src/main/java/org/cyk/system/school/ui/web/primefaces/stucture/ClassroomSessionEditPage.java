package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionEditPage extends AbstractCrudOnePage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = languageBusiness.findClassLabelText(AcademicSession.class)+" : "+identifiable.getAcademicSession().getUiString()
				+" - "+SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(identifiable);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		//@Input @InputOneChoice @InputOneCombo private AcademicSession academicSession;
		@Input @InputOneChoice @InputOneCombo private Teacher coordinator;
		
		
	}

}
