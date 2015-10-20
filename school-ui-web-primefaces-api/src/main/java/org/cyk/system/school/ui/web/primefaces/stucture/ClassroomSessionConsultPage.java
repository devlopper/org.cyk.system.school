package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.business.impl.CompanyReportRepository;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.UICommandable.CommandRequestType;
import org.cyk.ui.api.command.UICommandable.ViewType;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionConsultPage extends AbstractConsultPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = languageBusiness.findClassLabelText(ClassroomSession.class)+" : "+classroomSessionBusiness.format(identifiable);
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null);
		contextualMenu.setLabel(contentTitle); 
		for(ClassroomSessionDivision classroomSessionDivision : classroomSessionDivisionBusiness.findByClassroomSession(identifiable)){
			UICommandable division = UIProvider.getInstance().createCommandable("command.see.receipt", null);
			division.setCommandRequestType(CommandRequestType.UI_VIEW);
			division.setViewType(ViewType.TOOLS_REPORT);
			division.getParameters().addAll(navigationManager.reportParameters(identifiable, CompanyReportRepository.getInstance().getReportPointOfSale(),Boolean.FALSE));
			contextualMenu.getChildren().add(division);
		}
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
