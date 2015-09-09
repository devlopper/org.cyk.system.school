package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @Entity
public class EvaluationType extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private Subject subject;
	
	@ManyToOne private EvaluationTypeName name;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE)
	private BigDecimal coefficient;
	
	@Column(precision=COEFFICIENT_PRECISION*2,scale=FLOAT_SCALE)
	private BigDecimal maximumValue;
	
}
