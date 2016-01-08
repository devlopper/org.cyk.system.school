package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.StudentSubject;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentClassroomSessionDivision extends AbstractStudentResult<ClassroomSessionDivision,StudentSubject> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSessionDivision classroomSessionDivision;
	
	//@ManyToOne private StudentClassroomSession studentClassroomSession;
	
	/**/
		
	public StudentClassroomSessionDivision(Student student,ClassroomSessionDivision classroomSessionDivision) {
		super();
		this.student = student;
		this.classroomSessionDivision = classroomSessionDivision;
	}

	@Override
	public ClassroomSessionDivision getLevel() {
		return classroomSessionDivision;
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, student.getRegistration().getCode(),classroomSessionDivision.getIdentifier(),results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSessionDivision.class.getSimpleName()+"(STUD=%s DIV=%s %s)";
	
	public static final String FIELD_CLASSROOMSESSIONDIVISION = "classroomSessionDivision";
	
}
