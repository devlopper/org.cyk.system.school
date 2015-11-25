package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.event.EventBusiness;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class LectureConsultPage extends AbstractConsultPage<Lecture> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private EventBusiness eventBusiness;
	
	private FormOneData<Details> details;
	private Table<ParticipantDetails> participationTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		/*contentTitle = classroomSessionBusiness.format(identifiable.getClassroomSessionDivision().getClassroomSession())
				+" : "+classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision())
				+" : "+identifiable.getSubject().getName();*/
		
		
		
		details = createDetailsForm(Details.class, identifiable, new DetailsConfigurationListener.Form.Adapter<Lecture,Details>(Lecture.class, Details.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		participationTable = (Table<ParticipantDetails>) createDetailsTable(ParticipantDetails.class, new DetailsConfigurationListener.Table.Adapter<EventParticipation,ParticipantDetails>(EventParticipation.class, ParticipantDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<EventParticipation> getIdentifiables() {
				eventBusiness.load(identifiable.getEvent());
				return identifiable.getEvent().getEventParticipations();
			}	
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});

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
