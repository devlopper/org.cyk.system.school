package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.session.AcademicSessionDetails;
import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class AcademicSessionConsultPage extends AbstractConsultPage<AcademicSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<AcademicSessionDetails> details;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		details = createDetailsForm(AcademicSessionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<AcademicSession,AcademicSessionDetails>(AcademicSession.class, AcademicSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
					
	}

}
