package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.business.impl.adapter.ActorBusinessServiceAdapter;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDetails;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.command.IconType;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionConsultPage extends AbstractConsultPage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<StudentClassroomSessionDetails> details;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		
		details = createDetailsForm(StudentClassroomSessionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<StudentClassroomSession,StudentClassroomSessionDetails>(StudentClassroomSession.class, StudentClassroomSessionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTitleId() {
				return "model.entity.student";
			}
		});

	}
	
	@Override
	protected void processIdentifiableContextualCommandable(UICommandable commandable) {
		super.processIdentifiableContextualCommandable(commandable);
		if(ActorBusinessServiceAdapter.ARE_CUSTOMERS.contains(Student.class)){
			if(identifiable.getTuitionSale()==null)
				commandable.addChild(navigationManager.createUpdateCommandable(identifiable,"school.command.definetuition", IconType.THING_MONEY,SchoolWebManager.getInstance().getOutcomeDefineTuition()));
			else
				commandable.addChild(navigationManager.createConsultCommandable(identifiable.getTuitionSale(),"field.tuition", IconType.THING_MONEY));
		}
	}
	
}
