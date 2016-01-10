package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyk.system.root.business.api.event.EventBusiness;
import org.cyk.system.root.business.impl.event.EventParticipationDetails;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.subject.LectureDetails;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LectureConsultPage extends AbstractConsultPage<Lecture> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	@Inject private EventBusiness eventBusiness;
	
	private FormOneData<LectureDetails> details;
	private Table<EventParticipationDetails> participationTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		/*contentTitle = classroomSessionBusiness.format(identifiable.getClassroomSessionDivision().getClassroomSession())
				+" : "+classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision())
				+" : "+identifiable.getSubject().getName();*/
		
		
		
		details = createDetailsForm(LectureDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<Lecture,LectureDetails>(Lecture.class, LectureDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		participationTable = (Table<EventParticipationDetails>) createDetailsTable(EventParticipationDetails.class, new DetailsConfigurationListener.Table.Adapter<EventParticipation,EventParticipationDetails>(EventParticipation.class, EventParticipationDetails.class){
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

}
