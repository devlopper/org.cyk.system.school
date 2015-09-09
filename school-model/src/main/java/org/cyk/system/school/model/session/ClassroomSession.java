package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.Period;
import org.cyk.system.school.model.actor.Teacher;

@Getter @Setter @Entity @NoArgsConstructor
public class ClassroomSession extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne
	private AcademicSession academicSession;
	
	@ManyToOne
	private LevelTimeDivision levelTimeDivision;
	
	@Embedded private Period period;

	@ManyToOne private Teacher coordinator;
	
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
	
}
