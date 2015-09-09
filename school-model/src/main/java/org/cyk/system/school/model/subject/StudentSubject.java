package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentSubject extends AbstractStudentResult<Subject,EvaluatedStudent> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne
	private Subject subject;
	
	public StudentSubject(Student student, Subject subject) {
		super();
		this.student = student;
		this.subject = subject;
	}

	@Override
	public Subject getLevel() {
		return subject;
	}
	
	@Override
	public String toString() {
		return student+" "+subject;
	}
}
