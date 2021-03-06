package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.event.Event;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public class Lecture extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	@ManyToOne private Event event;

	public Lecture(ClassroomSessionDivisionSubject classroomSessionDivisionSubject, Event event) {
		super();
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		this.event = event;
	}
	
	public static final String FIELD_CLASSROOMSESSIONDIVISIONSUBJECT = "classroomSessionDivisionSubject";
	public static final String FIELD_EVENT = "event";
	
}
