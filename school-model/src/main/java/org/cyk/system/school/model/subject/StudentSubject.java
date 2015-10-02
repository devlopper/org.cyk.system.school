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
public class StudentSubject extends AbstractStudentResult<ClassroomSessionDivisionSubject,StudentSubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	public StudentSubject(Student student, ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
		super();
		this.student = student;
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
	}

	@Override
	public ClassroomSessionDivisionSubject getLevel() {
		return classroomSessionDivisionSubject;
	}
	
	@Override
	public String toString() {
		return student+" "+classroomSessionDivisionSubject;
	}
	
	public static final String FIELD_CLASSROOMSESSIONDIVISIONSUBJECT = "classroomSessionDivisionSubject";
}
