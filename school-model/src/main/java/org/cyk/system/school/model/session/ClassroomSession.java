package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSession extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private AcademicSession academicSession;
	
	@ManyToOne private LevelTimeDivision levelTimeDivision;
	
	@Embedded private Period period;

	@ManyToOne private Teacher coordinator;
	
	@Transient private Collection<ClassroomSessionDivision> divisions = new ArrayList<>();
	
	public ClassroomSession(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Period period,Teacher coordinator) {
		super();
		this.academicSession = academicSession;
		this.levelTimeDivision = levelTimeDivision;
		this.period = period;
		this.coordinator = coordinator;
	}
	
	@Override
	public String getUiString() {
		return levelTimeDivision.getUiString();
	}
	
	public static final String FIELD_ACADEMIC_DIVISION = "academicSession";
	public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
	public static final String FIELD_PERIOD = "period";
	public static final String FIELD_COORDINATOR = "coordinator";
	
}
