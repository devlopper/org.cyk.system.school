package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentClassroomSession extends AbstractStudentResult<ClassroomSession,StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSession classroomSession;
	
	public StudentClassroomSession(Student student,ClassroomSession classroomSession) {
		super();
		this.student = student;
		this.classroomSession = classroomSession;
	}

	@Override
	public ClassroomSession getLevel() {
		return classroomSession;
	}
	
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
}
