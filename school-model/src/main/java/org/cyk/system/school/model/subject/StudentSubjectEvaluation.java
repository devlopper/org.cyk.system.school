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
public class StudentSubjectEvaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private SubjectEvaluation subjectEvaluation;
	@ManyToOne @NotNull private StudentSubject studentSubject;
	@Column(precision=5,scale=FLOAT_SCALE) @NotNull private BigDecimal value;

	public StudentSubjectEvaluation(SubjectEvaluation subjectEvaluation,StudentSubject studentSubject, BigDecimal value) {
		super();
		this.subjectEvaluation = subjectEvaluation;
		this.studentSubject = studentSubject;
		this.value = value;
	}
	
	@Override
	public String toString() {
		return studentSubject.getStudent().getPerson()+":"+value;
	}
	
	public static final String FIELD_EVALUATION = "subjectEvaluation";
	public static final String FIELD_STUDENT_SUBJECT = "studentSubject";
	public static final String FIELD_VALUE = "value";
}
