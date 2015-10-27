package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @Entity @NoArgsConstructor
public class SubjectEvaluationType extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSessionDivisionSubject subject;
	
	@ManyToOne private EvaluationType type;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE)
	private BigDecimal coefficient;
	
	@Column(precision=COEFFICIENT_PRECISION*2,scale=FLOAT_SCALE)
	private BigDecimal maximumValue;

	public SubjectEvaluationType(ClassroomSessionDivisionSubject subject, EvaluationType type,BigDecimal coefficient, BigDecimal maximumValue) {
		super();
		this.subject = subject;
		this.type = type;
		this.coefficient = coefficient;
		this.maximumValue = maximumValue;
	}
	
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_TYPE = "type";
	
}
