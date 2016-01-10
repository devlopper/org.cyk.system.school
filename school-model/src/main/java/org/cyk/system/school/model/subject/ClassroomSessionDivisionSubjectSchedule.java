package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.time.Schedule;

@Getter @Setter @Entity
public class ClassroomSessionDivisionSubjectSchedule extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	@OneToOne private Schedule schedule;
	
	/**/
	
	public static final String FIELD_CLASSROOMSESSIONDIVISIONSUBJECT = "classroomSessionDivisionSubject";
	public static final String FIELD_SCHEDULE = "schedule";
}
