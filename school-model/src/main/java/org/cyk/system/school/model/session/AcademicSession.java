package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.event.AbstractIdentifiablePeriod;

@Getter @Setter @Entity @NoArgsConstructor
public class AcademicSession extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne private School school;
		
	@Embedded private CommonNodeInformations nodeInformations;
	
	@Temporal(value=TemporalType.DATE) private Date nextStartingDate;
	
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
