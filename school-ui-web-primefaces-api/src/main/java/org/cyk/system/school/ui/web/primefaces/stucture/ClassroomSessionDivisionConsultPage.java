package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.api.model.table.RowAdapter;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionConsultPage extends AbstractConsultPage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	
	private FormOneData<Details> classroomSessionDivisionDetails;
	private Table<DivisionSubjectDetails> classroomSessionDivisionSubjectTable;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		System.out
				.println("ClassroomSessionDivisionConsultPage.initialisation()");
		contentTitle = classroomSessionBusiness.format(identifiable.getClassroomSession())+" : "+classroomSessionDivisionBusiness.format(identifiable);
		
		classroomSessionDivisionDetails = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(classroomSessionDivisionDetails);
		
		classroomSessionDivisionSubjectTable = (Table<DivisionSubjectDetails>) createTable(DivisionSubjectDetails.class, null, null);
		configureDetailsTable(classroomSessionDivisionSubjectTable, "model.entity.classroomSessionDivisionSubject",Boolean.TRUE);
		
		classroomSessionDivisionSubjectTable.getRowListeners().add(new RowAdapter<DivisionSubjectDetails>(){
			@Override
			public void added(Row<DivisionSubjectDetails> row) {
				super.added(row);
				row.setOpenable(Boolean.TRUE);
				row.setUpdatable(Boolean.TRUE);
			}
		});
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		for(ClassroomSessionDivisionSubject classroomSessionDivisionSubject : identifiable.getSubjects())
			classroomSessionDivisionSubjectTable.addRow(new DivisionSubjectDetails(classroomSessionDivisionSubject));
		classroomSessionDivisionSubjectTable.setShowEditColumn(Boolean.TRUE);
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,duration;
		
		public Details(ClassroomSessionDivision classroomSessionDivision) {
			super(classroomSessionDivision);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(classroomSessionDivision);
			duration = timeBusiness.formatDuration(classroomSessionDivision.getDuration());
		}
		
	}
	
	public static class DivisionSubjectDetails extends AbstractOutputDetails<ClassroomSessionDivisionSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,coefficient,teacher;
		
		public DivisionSubjectDetails(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super(classroomSessionDivisionSubject);
			name = classroomSessionDivisionSubject.getSubject().getName();
			coefficient = numberBusiness.format(classroomSessionDivisionSubject.getCoefficient());
			teacher = classroomSessionDivisionSubject.getTeacher().getPerson().getNames();
		}
		
	}

}
