package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionSubject;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS,genderType=GenderType.MALE)
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {StudentClassroomSessionSubject.COLUMN_STUDENT
		, StudentClassroomSessionSubject.COLUMN_CLASSROOM_SESSION_SUBJECT})})
public class StudentClassroomSessionSubject extends AbstractStudentResult<ClassroomSessionSubject,StudentClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@ManyToOne @JoinColumn(name=COLUMN_CLASSROOM_SESSION_SUBJECT) @NotNull private ClassroomSessionSubject classroomSessionSubject;
	
	public StudentClassroomSessionSubject(Student student, ClassroomSessionSubject classroomSessionSubject) {
		super();
		this.student = student;
		this.classroomSessionSubject = classroomSessionSubject;
	}

	@Override
	public ClassroomSessionSubject getLevel() {
		return classroomSessionSubject;
	}
	
	@Override
	public String toString() {
		return student.getCode()+"|"+classroomSessionSubject;
		//return student+" "+classroomSessionDivisionSubject;
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, student.getCode(),classroomSessionSubject.getIdentifier(),results==null?Constant.EMPTY_STRING:results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSessionDivisionSubject.class.getSimpleName()+"(STUD=%s SUBJECT=%s %s)";
	
	public static final String FIELD_CLASSROOM_SESSION_SUBJECT = "classroomSessionSubject";
	
	/**/
	
	public static final String COLUMN_CLASSROOM_SESSION_SUBJECT = FIELD_CLASSROOM_SESSION_SUBJECT;
}
