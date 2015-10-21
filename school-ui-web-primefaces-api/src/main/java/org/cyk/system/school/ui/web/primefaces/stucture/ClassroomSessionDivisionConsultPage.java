package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	
	private Table<ClassroomSessionDivisionSubject> classroomSessionDivisionSubjectTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = languageBusiness.findClassLabelText(ClassroomSessionDivision.class)+" : "+classroomSessionDivisionBusiness.format(identifiable);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		/*for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisionBusiness.findByClassroomSession(identifiable)){
			commandable = navigationManager.createUpdateCommandable(classroomSessionDivision,"button",null);
			commandable.setLabel(classroomSessionDivisionBusiness.format(classroomSessionDivision));
			contextualMenu.getChildren().add(commandable);
		}*/
		return Arrays.asList(contextualMenu);
	}
	
	public static class Form extends AbstractOutputDetails<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String academicSession,coordinator;
		
		public Form(ClassroomSession classroomSession) {
			super(classroomSession);
			academicSession = classroomSession.getAcademicSession().getUiString();
			coordinator = classroomSession.getCoordinator().getPerson().getNames();
		}
		
	}

}
