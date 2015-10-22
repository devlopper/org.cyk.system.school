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
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
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
public class ClassroomSessionConsultPage extends AbstractConsultPage<ClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	
	private FormOneData<Details> classroomSessionDetails;
	private Table<DivisionDetails> classroomSessionDivisionTable;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = languageBusiness.findClassLabelText(ClassroomSession.class)+" : "+classroomSessionBusiness.format(identifiable);
		
		classroomSessionDetails = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(classroomSessionDetails);
		
		classroomSessionDivisionTable = (Table<DivisionDetails>) createTable(DivisionDetails.class, null, null);
		configureDetailsTable(classroomSessionDivisionTable, "model.entity.classroomSessionDivision",Boolean.TRUE);
		
		classroomSessionDivisionTable.getRowListeners().add(new RowAdapter<DivisionDetails>(){
			@Override
			public void added(Row<DivisionDetails> row) {
				super.added(row);
				row.setOpenable(Boolean.TRUE);
				row.setUpdatable(Boolean.TRUE);
			}
		});
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		for(ClassroomSessionDivision classroomSessionDivision : identifiable.getDivisions())
			classroomSessionDivisionTable.addRow(new DivisionDetails(classroomSessionDivision));
		
		classroomSessionDivisionTable.setShowEditColumn(Boolean.TRUE);
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		for(ClassroomSessionDivision classroomSessionDivision : identifiable.getDivisions()){
			commandable = navigationManager.createConsultCommandable(classroomSessionDivision,"button",null);
			commandable.setLabel(classroomSessionDivisionBusiness.format(classroomSessionDivision));
			contextualMenu.getChildren().add(commandable);
		}
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<ClassroomSession> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,coordinator;
		
		public Details(ClassroomSession classroomSession) {
			super(classroomSession);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(classroomSession);
			coordinator = classroomSession.getCoordinator().getPerson().getNames();
		}
		
	}
	
	public static class DivisionDetails extends AbstractOutputDetails<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,duration;
		
		public DivisionDetails(ClassroomSessionDivision classroomSessionDivision) {
			super(classroomSessionDivision);
			name = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(classroomSessionDivision);
			duration = timeBusiness.formatDuration(classroomSessionDivision.getDuration());
		}
		
	}

}
