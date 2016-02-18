package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.List;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.BusinessEntityInfos;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.FileExtension;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputManyPickList;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionMergeReportPage extends AbstractCrudOnePage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_STUDENTS, SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivisionIndex(
				SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex()));
	}
	
	@Override
	protected BusinessEntityInfos fetchBusinessEntityInfos() {
		return uiManager.businessEntityInfos(StudentClassroomSessionDivision.class);
	}
	
	@Override
	protected Crud crudFromRequestParameter() {
		return Crud.CREATE;
	}
			
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected void create() {
		navigationManager.redirectToFileConsultManyPage(SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
				.findReportFiles(((Form)form.getData()).getStudents()), FileExtension.PDF);
	}
	
	@Getter
	public static class Form extends AbstractFormModel<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputChoice(load=false) @InputManyChoice @InputManyPickList private List<StudentClassroomSessionDivision> students;
		
		public static final String FIELD_STUDENTS = "students";
	}

}
