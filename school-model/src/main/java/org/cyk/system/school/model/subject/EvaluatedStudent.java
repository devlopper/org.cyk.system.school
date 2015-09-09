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
public class EvaluatedStudent extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne
	private Evaluation evaluation;
	
	@ManyToOne
	private StudentSubject studentSubject;
	
	@Column(precision=5,scale=FLOAT_SCALE)
	private BigDecimal value;

	public EvaluatedStudent(StudentSubject studentSubject, BigDecimal value) {
		super();
		this.studentSubject = studentSubject;
		this.value = value;
	}
	
	@Override
	public String toString() {
		return studentSubject.getStudent().getPerson().getNames()+":"+value;
	}
	
}
