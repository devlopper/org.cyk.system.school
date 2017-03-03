package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.company.business.api.sale.SaleIdentifiableGlobalIdentifierBusiness;
import org.cyk.system.company.model.sale.SaleIdentifiableGlobalIdentifier;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.Icon;
import org.cyk.ui.api.command.AbstractCommandable;
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
		SaleIdentifiableGlobalIdentifier.SearchCriteria searchCriteria = new SaleIdentifiableGlobalIdentifier.SearchCriteria();
		searchCriteria.addIdentifiableGlobalIdentifier(identifiable);
		
		System.out.println("StudentClassroomSessionConsultPage.processIdentifiableContextualCommandable() : "
				+inject(SaleIdentifiableGlobalIdentifierBusiness.class).findByCriteria(searchCriteria));
		for(SaleIdentifiableGlobalIdentifier saleIdentifiableGlobalIdentifier : inject(SaleIdentifiableGlobalIdentifierBusiness.class).findByCriteria(searchCriteria)){
			commandable.addChild(AbstractCommandable.Builder.createConsult(saleIdentifiableGlobalIdentifier.getSale(), Icon.THING_MONEY));
		}
		/*if(ActorBusinessServiceAdapter.ARE_CUSTOMERS.contains(Student.class)){
			if(identifiable.getTuitionSale()==null)
				commandable.addChild(Builder.createCrud(Crud.UPDATE,identifiable,"school.command.definetuition", Icon.THING_MONEY,SchoolWebManager.getInstance().getOutcomeDefineTuition()));
			else
				commandable.addChild(Builder.createConsult(identifiable.getTuitionSale(), Icon.THING_MONEY));
		}*/
	}
	
}
