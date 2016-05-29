package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentClassroomSessionDivisionSubjectEvaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private Evaluation evaluation;
	@ManyToOne @NotNull private StudentClassroomSessionDivisionSubject studentSubject;
	@Column(precision=5,scale=FLOAT_SCALE) @NotNull private BigDecimal value;

	public StudentClassroomSessionDivisionSubjectEvaluation(Evaluation evaluation,StudentClassroomSessionDivisionSubject studentSubject, BigDecimal value) {
		super();
		this.evaluation = evaluation;
		this.studentSubject = studentSubject;
		this.value = value;
	}
	/*
	@Override
	public String toString() {
		return subjectEvaluation.getType().getSubject().getIdentifier()+":"+studentSubject.getStudent().getRegistration().getCode()+":"+value;
	}*/
	
	public static final String FIELD_EVALUATION = "evaluation";
	public static final String FIELD_STUDENT_SUBJECT = "studentSubject";
	public static final String FIELD_VALUE = "value";
}
