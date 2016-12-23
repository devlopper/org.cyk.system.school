package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import org.cyk.system.school.model.AbstractNodeAggregatedResult;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.subject.Subject;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor
public class SubjectClassroomSession extends AbstractNodeAggregatedResult implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @NotNull private Subject subject;
	@ManyToOne @NotNull private ClassroomSession classroomSession;
	
	@ManyToOne private Teacher teacher;
	
	public SubjectClassroomSession(Subject subject,ClassroomSession classroomSession) {
		super();
		this.subject = subject;
		this.classroomSession = classroomSession;
	}
	
	@Override
	public String getUiString() {
		return subject.getUiString();
	}

	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
}
