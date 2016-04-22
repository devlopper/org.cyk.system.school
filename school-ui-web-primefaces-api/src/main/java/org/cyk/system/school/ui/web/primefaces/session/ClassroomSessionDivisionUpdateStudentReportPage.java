package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyCheck;
import org.cyk.utility.common.annotation.user.interfaces.InputManyChoice;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionUpdateStudentReportPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		text = languageBusiness.findText("org.cyk.system.school.ui.web.primefaces.session.ClassroomSessionDivisionUpdateStudentReportPage.text", new Object[]{
				formatUsingBusiness(identifiable.getClassroomSession()),formatUsingBusiness(identifiable)
		});
		form.getSubmitCommandable().setLabel(text("command.update"));
		form.getSubmitCommandable().getCommand().setConfirm(Boolean.TRUE);
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_STUDENT_CLASSROOM_SESSION_DIVISIONS
				, SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivision(identifiable));
	}
	
	@Override
	protected void update() {
		//SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().buildReport(Arrays.asList(identifiable));
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = instanciateCommandableBuilder().setLabel(formatUsingBusiness(identifiable)).create();
	
		contextualMenu.getChildren().add(Builder.createConsult(identifiable.getClassroomSession(), null));
		
		return Arrays.asList(contextualMenu);
	}
		
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	public static class Form extends AbstractFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice(load=false) @InputManyChoice @InputManyCheck private List<StudentClassroomSessionDivision> studentClassroomSessionDivisions;
		
		public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISIONS = "studentClassroomSessionDivisions";
	}

}
