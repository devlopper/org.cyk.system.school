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
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS,genderType=GenderType.MALE)
public class StudentClassroomSessionDivisionSubject extends AbstractStudentResult<ClassroomSessionDivisionSubject,StudentClassroomSessionDivisionSubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne @NotNull private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	public StudentClassroomSessionDivisionSubject(Student student, ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
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
		return String.format(LOG_FORMAT, student.getCode(),classroomSessionDivisionSubject.getIdentifier(),results==null?Constant.EMPTY_STRING:results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSessionDivisionSubject.class.getSimpleName()+"(STUD=%s SUBJECT=%s %s)";
	
	public static final String FIELD_CLASSROOMSESSIONDIVISIONSUBJECT = "classroomSessionDivisionSubject";
}
