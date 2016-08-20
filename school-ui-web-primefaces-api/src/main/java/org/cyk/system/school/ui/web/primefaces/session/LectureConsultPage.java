package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.event.EventPartyBusiness;
import org.cyk.system.root.business.impl.event.EventPartyDetails;
import org.cyk.system.root.model.event.EventParty;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LectureConsultPage extends AbstractConsultPage<Lecture> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<EventPartyDetails> participationTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		participationTable = (Table<EventPartyDetails>) createDetailsTable(EventPartyDetails.class, new DetailsConfigurationListener.Table.Adapter<EventParty,EventPartyDetails>(EventParty.class, EventPartyDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<EventParty> getIdentifiables() {
				return inject(EventPartyBusiness.class).findByEvent(identifiable.getEvent());
			}	
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});

	}

}
