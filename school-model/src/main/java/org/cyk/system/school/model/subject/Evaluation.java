package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @Entity
public class Evaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@JoinColumn(name="etype") @ManyToOne private EvaluationType type;
	
	@ManyToOne private Subject subject;
	
	@Temporal(TemporalType.TIMESTAMP) @Column(name="happeningDate") private Date date;
	
	/*
	@NotNull(groups=Client.class)
	private Boolean coefficientApplied;
	*/
	/**/
	
	@Transient private Collection<EvaluatedStudent> evaluatedStudents;
	
}
