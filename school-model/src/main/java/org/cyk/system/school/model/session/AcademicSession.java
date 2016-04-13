package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.event.AbstractIdentifiablePeriod;
import org.cyk.system.root.model.mathematics.machine.FiniteStateMachineState;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS,genderType=GenderType.FEMALE)
public class AcademicSession extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne @NotNull private School school;
		
	@Embedded private CommonNodeInformations nodeInformations;
	
	@Temporal(value=TemporalType.DATE) private Date nextStartingDate;
	
	@ManyToOne private FiniteStateMachineState finiteStateMachineState;
	
	public AcademicSession(School school,CommonNodeInformations nodeInformations,Date nextStartingDate) {
		super();
		this.school = school;
		this.nodeInformations = nodeInformations;
		this.nextStartingDate = nextStartingDate;
	} 
	
	@Override
	public String getUiString() {
		return period.getUiString();
	}
}
