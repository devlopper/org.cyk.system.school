package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;

@Getter @Setter @Entity @NoArgsConstructor
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {StudentClassroomSessionDivisionSubjectEvaluation.COLUMN_EVALUATION
		, StudentClassroomSessionDivisionSubjectEvaluation.COLUMN_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT})})
public class StudentClassroomSessionDivisionSubjectEvaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_EVALUATION) @NotNull private Evaluation evaluation;
	@ManyToOne @JoinColumn(name=COLUMN_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT) @NotNull private StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject;
	@Column(name="thevalue",precision=5,scale=FLOAT_SCALE) @NotNull private BigDecimal value;

	public StudentClassroomSessionDivisionSubjectEvaluation(Evaluation evaluation,StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject, BigDecimal value) {
		super();
		this.evaluation = evaluation;
		this.studentClassroomSessionDivisionSubject = studentClassroomSessionDivisionSubject;
		this.value = value;
	}
	
	public static final String FIELD_EVALUATION = "evaluation";
	public static final String FIELD_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT = "studentClassroomSessionDivisionSubject";
	public static final String FIELD_VALUE = "value";
	
	/**/
	
	public static final String COLUMN_EVALUATION = "evaluation";
	public static final String COLUMN_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT = "studentClassroomSessionDivisionSubject";
	
}
