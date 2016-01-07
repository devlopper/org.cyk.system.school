package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.company.model.structure.OwnedCompany;
import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.machine.FiniteStateMachine;

@Getter @Setter @Entity @NoArgsConstructor
public class School extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@OneToOne @NotNull private OwnedCompany ownedCompany;
	
	@Embedded private CommonNodeInformations nodeInformations;

	@ManyToOne private FiniteStateMachine finiteStateMachine;
	
	public School(OwnedCompany ownedCompany,CommonNodeInformations nodeInformations) {
		super();
		this.ownedCompany = ownedCompany;
		this.nodeInformations = nodeInformations;
	}
	
	public static final String FIELD_OWNED_COMPANY = "ownedCompany";
	public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
	
}
