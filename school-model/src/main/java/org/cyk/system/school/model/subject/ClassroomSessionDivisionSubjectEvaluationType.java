package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.mathematics.Interval;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {ClassroomSessionDivisionSubjectEvaluationType.COLUMN_CLASSROOM_SESSION_DIVISION_SUBJECT
		, ClassroomSessionDivisionSubjectEvaluationType.COLUMN_EVALUATION_TYPE})})
public class ClassroomSessionDivisionSubjectEvaluationType extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_CLASSROOM_SESSION_DIVISION_SUBJECT) @NotNull private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	@ManyToOne @JoinColumn(name=COLUMN_EVALUATION_TYPE) @NotNull private EvaluationType evaluationType;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE,nullable=false) @NotNull private BigDecimal maximumValue;
	
	@OneToOne private Interval countInterval;
	
	@Column(nullable=false) @NotNull private Long numberOfEvaluations=0l;
	
	public ClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubject classroomSessionDivisionSubject, EvaluationType evaluationType,BigDecimal coefficient, BigDecimal maximumValue) {
		super();
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		this.evaluationType = evaluationType;
		setWeight(coefficient);
		this.maximumValue = maximumValue;
	}
	
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
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
	public static final String FIELD_EVALUATION_TYPE = "evaluationType";
	
	/**/
	
	public static final String COLUMN_CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
	public static final String COLUMN_EVALUATION_TYPE = "evaluationType";
}
