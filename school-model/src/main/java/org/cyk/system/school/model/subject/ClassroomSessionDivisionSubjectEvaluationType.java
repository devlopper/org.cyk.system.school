package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.Interval;

@Getter @Setter @Entity @NoArgsConstructor
public class ClassroomSessionDivisionSubjectEvaluationType extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	@ManyToOne @NotNull private EvaluationType evaluationType;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE,nullable=false) @NotNull private BigDecimal coefficient;
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE,nullable=false) @NotNull private BigDecimal maximumValue;
	@OneToOne private Interval countInterval;
	@Column(nullable=false) @NotNull private Long numberOfEvaluations=0l;
	
	public ClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubject classroomSessionDivisionSubject, EvaluationType evaluationType,BigDecimal coefficient, BigDecimal maximumValue) {
		super();
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		this.evaluationType = evaluationType;
		this.coefficient = coefficient;
		this.maximumValue = maximumValue;
	}
	
	public static final String FIELD_SUBJECT = "classroomSessionDivisionSubject";
	public static final String FIELD_TYPE = "evaluationType";
	
	@Override
	public String getUiString() {
		return evaluationType.getUiString();
	}
	
	@Override
	public String toString() {
		return evaluationType.toString();
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, classroomSessionDivisionSubject.getIdentifier(),evaluationType.getIdentifier());
	}
	private static final String LOG_FORMAT = ClassroomSessionDivisionSubjectEvaluationType.class.getSimpleName()+"(SUBJECT=%s TYPE=%s)";
	
	public static final String FIELD_NUMBER_OF_EVALUATIONS = "numberOfEvaluations";
}
