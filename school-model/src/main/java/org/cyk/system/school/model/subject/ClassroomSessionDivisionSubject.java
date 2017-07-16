package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.root.model.time.ScheduleCollection;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(genderType=GenderType.FEMALE,crudStrategy=CrudStrategy.BUSINESS)
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {ClassroomSessionDivisionSubject.COLUMN_CLASSROOM_SESSION_DIVISION
		, ClassroomSessionDivisionSubject.COLUMN_SUBJECT})})
public class ClassroomSessionDivisionSubject extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_CLASSROOM_SESSION_DIVISION) @NotNull private ClassroomSessionDivision classroomSessionDivision;
	
	@ManyToOne @JoinColumn(name=COLUMN_SUBJECT) @NotNull private Subject subject;
		
	@ManyToOne private ClassroomSessionDivisionSubjectGroup group;
	
	@ManyToOne private Teacher teacher;
	
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal evaluationPassAverage;
	
	@Column(nullable=false) @NotNull private Long numberOfEvaluationTypes=0l;
	
	@Embedded private NodeResults results = new NodeResults();
	
	@Transient private IdentifiableRuntimeCollection<ClassroomSessionDivisionSubjectEvaluationType> evaluationTypes;
	@Transient private Boolean autoCreateStudentClassroomSessionDivisionSubject;
	
	/**
	 * The time table
	 */
	//@ManyToOne private ScheduleCollection scheduleCollection;
	
	@Transient private Collection<Evaluation> evaluations = new ArrayList<>();
	@Transient private Collection<Lecture> lectures = new ArrayList<>();
	
	public ClassroomSessionDivisionSubject(ClassroomSessionDivision classroomSessionDivision,Subject subject, BigDecimal weight,Teacher teacher) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
		this.subject = subject;
		getGlobalIdentifierCreateIfNull().setWeight(weight);
		this.teacher = teacher;
	}
	
	public NodeResults getResults(){
		if(results==null)
			results = new NodeResults();
		return results;
	}
	
	public IdentifiableRuntimeCollection<ClassroomSessionDivisionSubjectEvaluationType> getEvaluationTypes(){
		if(evaluationTypes == null)
			evaluationTypes = new IdentifiableRuntimeCollection<>();
		return evaluationTypes;
	}
	
	@Override
	public String toString() {
		return classroomSessionDivision.getCode()+"|"+subject.getCode();
	}
	
	@Override
	public String getUiString() {
		return subject.getUiString();
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, classroomSessionDivision.getIdentifier(),subject.getCode(),teacher==null?Constant.EMPTY_STRING:teacher.getCode(),results.getLogMessage());
	}
	private static final String LOG_FORMAT = ClassroomSessionDivisionSubject.class.getSimpleName()+"(DIV=%s SUB=%s TEACH=%s %s)";
	
	/**/

	public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_TEACHER = "teacher";
	public static final String FIELD_NUMBER_OF_EVALUATION_TYPES = "numberOfEvaluationTypes";
	
	/**/
	
	public static final String COLUMN_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	public static final String COLUMN_SUBJECT = "subject";
}
