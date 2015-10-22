package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.event.AbstractIdentifiablePeriod;

@Getter @Setter @Entity @NoArgsConstructor
public class AcademicSession extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne private School school;
		
	@Embedded private CommonNodeInformations nodeInformations;
	
	public AcademicSession(School school,CommonNodeInformations nodeInformations) {
		super();
		this.school = school;
		this.nodeInformations = nodeInformations;
	} 
	
	@Override
	public String getUiString() {
		return period.getUiString();
	}
}
