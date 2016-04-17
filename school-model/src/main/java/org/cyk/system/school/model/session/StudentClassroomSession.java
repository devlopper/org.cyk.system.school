package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.company.model.sale.Sale;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.Constant;

@Getter @Setter @Entity @NoArgsConstructor
public class StudentClassroomSession extends AbstractStudentResult<ClassroomSession,StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSession classroomSession;
	@ManyToOne private Sale tuitionSale;
	
	public StudentClassroomSession(Student student,ClassroomSession classroomSession) {
		super();
		this.student = student;
		this.classroomSession = classroomSession;
	}

	@Override
	public ClassroomSession getLevel() {
		return classroomSession;
	}
	
	@Override
	public String toString() {
		return student+" "+classroomSession;
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, student.getRegistration().getCode(),classroomSession.getIdentifier(),results==null?Constant.EMPTY_STRING:results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSession.class.getSimpleName()+"(STUD=%s CLASS=%s %s)";
	
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
}
