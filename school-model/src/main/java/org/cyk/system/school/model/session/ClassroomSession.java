package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.event.AbstractIdentifiablePeriod;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public class ClassroomSession extends AbstractIdentifiablePeriod implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private AcademicSession academicSession;
	
	@ManyToOne private LevelTimeDivision levelTimeDivision;
	
	@ManyToOne private Teacher coordinator;
	
	@Transient private Collection<ClassroomSessionDivision> divisions = new ArrayList<>();
	
	public ClassroomSession(AcademicSession academicSession,LevelTimeDivision levelTimeDivision,Teacher coordinator) {
		super();
		this.academicSession = academicSession;
		this.levelTimeDivision = levelTimeDivision;
		this.coordinator = coordinator;
	}
	
	@Override
	public String getUiString() {
		return levelTimeDivision.getUiString();
	}
	
	public static final String FIELD_ACADEMIC_DIVISION = "academicSession";
	public static final String FIELD_LEVEL_TIME_DIVISION = "levelTimeDivision";
	public static final String FIELD_COORDINATOR = "coordinator";
	
}
