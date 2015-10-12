package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.root.model.time.Period;

@Getter @Setter @Entity @NoArgsConstructor
public class AcademicSession extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne private School school;
	
	@Embedded private Period period;
	
	@Embedded private CommonNodeInformations nodeInformations;
	
	@ManyToOne private Person staffPersonSigner;
	
	public AcademicSession(School school,Period period,Person staffPersonSigner) {
		super();
		this.school = school;
		this.period = period;
		this.staffPersonSigner = staffPersonSigner;
	} 
	
}
