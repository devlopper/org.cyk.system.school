package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.company.model.structure.OwnedCompany;
import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @Entity @NoArgsConstructor
public class School extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@OneToOne @NotNull private OwnedCompany ownedCompany;
	
	@Embedded private CommonNodeInformations nodeInformations = new CommonNodeInformations();

	public School(OwnedCompany ownedCompany) {
		super();
		this.ownedCompany = ownedCompany;
	}
	
	public CommonNodeInformations getNodeInformations(){
		if(nodeInformations==null)
			nodeInformations = new CommonNodeInformations();
		return nodeInformations;
	}
	
	public static final String FIELD_OWNED_COMPANY = "ownedCompany";
	public static final String FIELD_NODE_INFORMATIONS = "nodeInformations";
	
}
