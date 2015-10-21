package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.ScheduleCollection;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSessionDivisionSubject extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private ClassroomSessionDivision classroomSessionDivision;
	
	@ManyToOne @NotNull private Subject subject;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE) @NotNull private BigDecimal coefficient;
	
	@ManyToOne private SubjectGroup group;
	
	@ManyToOne private Teacher teacher;
	
	@Column(precision=5,scale=FLOAT_SCALE)
	private BigDecimal duration;
	
	//TODO /* I think can be modeled using automated finite state machine */
	
	@Temporal(TemporalType.TIMESTAMP) private Date closingDateOfEvaluations;
	
	@Temporal(TemporalType.TIMESTAMP) private Date closingDateOfLectures;
	
	@Temporal(TemporalType.TIMESTAMP) private Date computingDateOfResults;
	
	/**
	 * The time table
	 */
	@ManyToOne private ScheduleCollection scheduleCollection;
	
	public ClassroomSessionDivisionSubject(ClassroomSessionDivision classroomSessionDivision,Subject subject, BigDecimal coefficient,Teacher teacher) {
		super();
		this.classroomSessionDivision = classroomSessionDivision;
		this.subject = subject;
		this.coefficient = coefficient;
		this.teacher = teacher;
	}
	
	@Override
	public String toString() {
		return subject.toString();
	}
	
	/**/

	public static final String FIELD_CLASSROOMSESSIONDIVISION = "classroomSessionDivision";
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_TEACHER = "teacher";
	
}
