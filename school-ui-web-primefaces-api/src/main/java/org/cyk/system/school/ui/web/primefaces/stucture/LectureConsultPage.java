package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.event.EventBusiness;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.subject.Lecture;
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

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LectureConsultPage extends AbstractConsultPage<Lecture> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private EventBusiness eventBusiness;
	
	private FormOneData<Details> lectureDetails;
	private Table<ParticipantDetails> participationTable;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		/*contentTitle = classroomSessionBusiness.format(identifiable.getClassroomSessionDivision().getClassroomSession())
				+" : "+classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision())
				+" : "+identifiable.getSubject().getName();*/
		
		eventBusiness.load(identifiable.getEvent());
		
		lectureDetails = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(lectureDetails);
		
		participationTable = (Table<ParticipantDetails>) createTable(ParticipantDetails.class, null, null);
		configureDetailsTable(participationTable, "model.entity.student",Boolean.TRUE);
		
		participationTable.getRowListeners().add(new RowAdapter<ParticipantDetails>(){
			@Override
			public void added(Row<ParticipantDetails> row) {
				super.added(row);
				row.setOpenable(Boolean.TRUE);
				row.setUpdatable(Boolean.TRUE);
			}
		});
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		for(EventParticipation eventParticipation : identifiable.getEvent().getEventParticipations()){
			participationTable.addRow(new ParticipantDetails(eventParticipation));	
		}
		//classroomSessionDivisionSubjectTable.setShowEditColumn(Boolean.TRUE);
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		/*commandable = navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivision(), "button", null);
		commandable.setLabel(classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision()));
		contextualMenu.getChildren().add(commandable);*/
		
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		
		/*commandable = navigationManager.createCreateCommandable(Lecture.class, uiManager.businessEntityInfos(Lecture.class).getUiLabelId(), null);
		commandable.getParameters().add(new Parameter(uiManager.businessEntityInfos(ClassroomSessionDivisionSubject.class), iden));
		contextualMenu.getChildren().add(commandable);*/
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<Lecture> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String date;
		
		public Details(Lecture lecture) {
			super(lecture);
			date = timeBusiness.formatDate(lecture.getEvent().getPeriod().getFromDate());
		}
		
	}
	
	public static class ParticipantDetails extends AbstractOutputDetails<EventParticipation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String names,present;
		
		public ParticipantDetails(EventParticipation eventParticipation) {
			super(eventParticipation);
			names = ((Person)eventParticipation.getParty()).getNames();
		}
		
	}

}
