package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.company.business.impl.adapter.ActorBusinessServiceAdapter;
import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.Icon;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionConsultPage extends AbstractConsultPage<StudentClassroomSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void processIdentifiableContextualCommandable(UICommandable commandable) {
		super.processIdentifiableContextualCommandable(commandable);
		if(ActorBusinessServiceAdapter.ARE_CUSTOMERS.contains(Student.class)){
			if(identifiable.getTuitionSale()==null)
				commandable.addChild(Builder.createCrud(Crud.UPDATE,identifiable,"school.command.definetuition", Icon.THING_MONEY,SchoolWebManager.getInstance().getOutcomeDefineTuition()));
			else
				commandable.addChild(Builder.createConsult(identifiable.getTuitionSale()/*,"field.tuition"*/, Icon.THING_MONEY));
		}
	}
	
}
