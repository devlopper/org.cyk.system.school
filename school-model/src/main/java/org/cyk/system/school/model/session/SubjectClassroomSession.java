package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.AbstractNodeAggregatedResult;
import org.cyk.system.school.model.subject.Subject;

@Getter @Setter @Entity @NoArgsConstructor
public class SubjectClassroomSession extends AbstractNodeAggregatedResult implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne private Subject subject;
	@ManyToOne private ClassroomSession classroomSession;
	
	public SubjectClassroomSession(Subject subject,ClassroomSession classroomSession) {
		super();
		this.subject = subject;
		this.classroomSession = classroomSession;
	}

	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
}
