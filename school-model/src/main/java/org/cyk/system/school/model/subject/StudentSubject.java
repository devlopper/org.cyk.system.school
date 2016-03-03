package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.Constant;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentSubject extends AbstractStudentResult<ClassroomSessionDivisionSubject,StudentSubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne @NotNull private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
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
		return getLogMessage();
		//return student+" "+classroomSessionDivisionSubject;
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, student.getRegistration().getCode(),classroomSessionDivisionSubject.getIdentifier(),results==null?Constant.EMPTY_STRING:results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentSubject.class.getSimpleName()+"(STUD=%s SUBJECT=%s %s)";
	
	public static final String FIELD_CLASSROOMSESSIONDIVISIONSUBJECT = "classroomSessionDivisionSubject";
}
