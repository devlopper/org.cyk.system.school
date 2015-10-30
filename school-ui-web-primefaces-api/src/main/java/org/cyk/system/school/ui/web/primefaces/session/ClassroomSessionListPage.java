package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudManyPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionListPage extends AbstractCrudManyPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		rowAdapter.setOpenable(Boolean.TRUE);
		rowAdapter.setUpdatable(Boolean.TRUE);
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		table.setShowOpenCommand(Boolean.TRUE);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractOutputDetails<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,coordinator;
		
		public Form(ClassroomSession classroomSession) {
			super(classroomSession);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(classroomSession);
			coordinator = classroomSession.getCoordinator().getPerson().getNames();
		}
	}

}
