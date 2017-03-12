package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.search.AbstractFieldValueSearchCriteriaSet;
import org.cyk.system.root.model.search.IntegerSearchCriteria;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.StudentResults;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS,genderType=GenderType.MALE)
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {StudentClassroomSession.COLUMN_STUDENT
		, StudentClassroomSession.COLUMN_CLASSROOM_SESSION})})
public class StudentClassroomSession extends AbstractStudentResult<ClassroomSession,StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_CLASSROOM_SESSION) @NotNull private ClassroomSession classroomSession;
	
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
		return String.format(LOG_FORMAT, student.getCode(),classroomSession==null?Constant.EMPTY_STRING:classroomSession.getIdentifier(),results==null?Constant.EMPTY_STRING:results.getLogMessage());
	}
	private static final String LOG_FORMAT = StudentClassroomSession.class.getSimpleName()+"(STUD=%s CLASS=%s %s)";
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	
	/**/
	
	public static final String COLUMN_CLASSROOM_SESSION = "classroomSession";
	
	@Getter @Setter
	public static class SearchCriteria extends AbstractFieldValueSearchCriteriaSet.AbstractIdentifiableSearchCriteriaSet implements Serializable {

		private static final long serialVersionUID = 6796076474234170332L;

		private StudentResults.SearchCriteria results = new StudentResults.SearchCriteria();
		private IntegerSearchCriteria divisionCount = new IntegerSearchCriteria();
		private Set<Integer> divisionIndexesRequired = new LinkedHashSet<>();
		private Set<ClassroomSession> classroomSessions = new LinkedHashSet<>();
		
		public SearchCriteria addClassroomSessions(Collection<ClassroomSession> classroomSessions){
			this.classroomSessions.addAll(classroomSessions);
			return this;
		}
	}
}
