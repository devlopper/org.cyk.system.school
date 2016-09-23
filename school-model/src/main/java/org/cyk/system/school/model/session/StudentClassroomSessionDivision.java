package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

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
public class StudentClassroomSessionDivision extends AbstractStudentResult<ClassroomSessionDivision,StudentClassroomSessionDivisionSubject> implements Serializable {

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
	public String toString() {
		return student+" "+classroomSessionDivision;
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, student.getCode(),classroomSessionDivision.getIdentifier(),results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSessionDivision.class.getSimpleName()+"(STUD=%s DIV=%s %s)";
	
	public static final String FIELD_CLASSROOMSESSIONDIVISION = "classroomSessionDivision";
	
	/**/
	/*
	@Getter @Setter
	public static class SearchCriteria extends AbstractFieldValueSearchCriteriaSet implements Serializable {

		private static final long serialVersionUID = 6796076474234170332L;

		private StudentResults.SearchCriteria results = new StudentResults.SearchCriteria();
		private Set<Integer> classroomSessionDivisionRequiredIndexes = new LinkedHashSet<>();
		
	}*/

}
