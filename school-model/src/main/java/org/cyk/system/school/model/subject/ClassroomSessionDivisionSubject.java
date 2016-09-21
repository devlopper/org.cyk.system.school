package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.ScheduleCollection;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(genderType=GenderType.FEMALE,crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSessionDivisionSubject extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private ClassroomSessionDivision classroomSessionDivision;
	
	@ManyToOne @NotNull private Subject subject;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE,nullable=false) @NotNull private BigDecimal coefficient;
	
	@ManyToOne private ClassroomSessionDivisionSubjectGroup group;
	
	@ManyToOne private Teacher teacher;
	
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal duration;
	
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal evaluationPassAverage;
	
	@Column(nullable=false) @NotNull private Long numberOfEvaluationTypes=0l;
	
	@Embedded private NodeResults results = new NodeResults();
	
	/**
	 * The time table
	 */
	@ManyToOne private ScheduleCollection scheduleCollection;
	
	@Transient private Collection<Evaluation> evaluations = new ArrayList<>();
	@Transient private Collection<Lecture> lectures = new ArrayList<>();
	
	public ClassroomSessionDivisionSubject(ClassroomSessionDivision classroomSessionDivision,Subject subject, BigDecimal coefficient,Teacher teacher) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
		this.subject = subject;
		this.coefficient = coefficient;
		this.teacher = teacher;
	}
	
	public NodeResults getResults(){
		if(results==null)
			results = new NodeResults();
		return results;
	}
	
	@Override
	public String toString() {
		return subject.toString();
	}
	
	@Override
	public String getUiString() {
		return subject.getUiString();
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, classroomSessionDivision.getIdentifier(),subject.getCode(),coefficient,teacher==null?Constant.EMPTY_STRING:teacher.getCode(),duration,results.getLogMessage());
	}
	private static final String LOG_FORMAT = ClassroomSessionDivisionSubject.class.getSimpleName()+"(DIV=%s SUB=%s COEF=%s TEACH=%s DUR=%s %s)";
	
	/**/

	public static final String FIELD_CLASSROOMSESSIONDIVISION = "classroomSessionDivision";
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_TEACHER = "teacher";
	public static final String FIELD_DURATION = "duration";
	public static final String FIELD_NUMBER_OF_EVALUATION_TYPES = "numberOfEvaluationTypes";
	
}
