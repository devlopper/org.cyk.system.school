package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.NodeResults;

@Getter @Setter @Entity @NoArgsConstructor
public class ClassroomSessionDivision extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSession classroomSession;
	
	@Embedded private Period period;
	
	@ManyToOne private TimeDivisionType timeDivisionType;
	
	@Column(precision=5,scale=FLOAT_SCALE)
	private BigDecimal duration;
	
	@Column(precision=COEFFICIENT_PRECISION,scale=FLOAT_SCALE)
	private BigDecimal coefficient;
	
	@Embedded private NodeResults results = new NodeResults();

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
	
}
