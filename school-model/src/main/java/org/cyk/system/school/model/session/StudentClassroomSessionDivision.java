package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS,genderType=GenderType.MALE)
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {StudentClassroomSessionDivision.COLUMN_STUDENT
		, StudentClassroomSessionDivision.COLUMN_CLASSROOM_SESSION_DIVISION})})
public class StudentClassroomSessionDivision extends AbstractStudentResult<ClassroomSessionDivision,StudentClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_CLASSROOM_SESSION_DIVISION) @NotNull private ClassroomSessionDivision classroomSessionDivision;
	
	@Transient @Setter private IdentifiableRuntimeCollection<StudentClassroomSessionDivisionSubject> studentClassroomSessionDivisionSubjects;
	
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
	
	public IdentifiableRuntimeCollection<StudentClassroomSessionDivisionSubject> getStudentClassroomSessionDivisionSubjects(){
		if(studentClassroomSessionDivisionSubjects==null)
			studentClassroomSessionDivisionSubjects = new IdentifiableRuntimeCollection<>();
		return studentClassroomSessionDivisionSubjects;
	}
	
	@Override
	public String toString() {
		return getCode(); //student+" "+classroomSessionDivision;
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, student.getCode(),classroomSessionDivision.getIdentifier(),results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSessionDivision.class.getSimpleName()+"(STUD=%s DIV=%s %s)";
	
	public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	
	/**/
	
	public static final String COLUMN_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	
	/*
	@Getter @Setter
	public static class SearchCriteria extends AbstractFieldValueSearchCriteriaSet implements Serializable {

		private static final long serialVersionUID = 6796076474234170332L;

		private StudentResults.SearchCriteria results = new StudentResults.SearchCriteria();
		private Set<Integer> classroomSessionDivisionRequiredIndexes = new LinkedHashSet<>();
		
	}*/

}
