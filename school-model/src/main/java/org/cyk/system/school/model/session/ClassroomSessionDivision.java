package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.NodeResults;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSessionDivision extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSession classroomSession;
	
	@Embedded private Period period;
	
	@ManyToOne private TimeDivisionType timeDivisionType;
	
	private Long duration;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE)
	private BigDecimal coefficient;
	
	@Embedded private NodeResults results = new NodeResults();

	@Transient private Collection<ClassroomSessionDivisionSubject> subjects = new ArrayList<>();
	
	public ClassroomSessionDivision(ClassroomSession classroomSession,TimeDivisionType timeDivisionType,Period period, BigDecimal coefficient) {
		super();
		this.timeDivisionType = timeDivisionType;
		this.classroomSession = classroomSession;
		this.period = period;
		this.coefficient = coefficient;
	}
	
	@Override
	public String getUiString() {
		return timeDivisionType.getUiString();
	}
	
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
	public static final String FIELD_PERIOD = "period";
	public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
	public static final String FIELD_DURATION = "duration";
	public static final String FIELD_COEFFICIENT = "coefficient";
	public static final String FIELD_RESULTS = "results";
	
}
