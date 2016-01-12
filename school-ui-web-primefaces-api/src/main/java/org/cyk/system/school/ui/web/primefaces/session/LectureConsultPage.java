package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.event.EventParticipationDetails;
import org.cyk.system.root.model.event.EventParticipation;
import org.cyk.system.school.business.impl.subject.LectureDetails;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class LectureConsultPage extends AbstractConsultPage<Lecture> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<LectureDetails> details;
	private Table<EventParticipationDetails> participationTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
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

}
