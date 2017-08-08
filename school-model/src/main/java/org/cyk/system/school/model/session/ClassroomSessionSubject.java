package org.cyk.system.school.model.session;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import org.cyk.system.school.model.AbstractNodeAggregatedResult;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.subject.Subject;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @Entity @NoArgsConstructor
@Table(uniqueConstraints={@UniqueConstraint(columnNames = {ClassroomSessionSubject.FIELD_CLASSROOMSESSION,ClassroomSessionSubject.FIELD_SUBJECT})})
public class ClassroomSessionSubject extends AbstractNodeAggregatedResult implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_CLASSROOMSESSION) @NotNull private ClassroomSession classroomSession;
	@ManyToOne @JoinColumn(name=COLUMN_SUBJECT) @NotNull private Subject subject;
	
	@ManyToOne @JoinColumn(name=COLUMN_TEACHER) private Teacher teacher;
	
	public ClassroomSessionSubject(ClassroomSession classroomSession,Subject subject) {
		super();
		this.classroomSession = classroomSession;
		this.subject = subject;
	}
	
	@Override
	public String getUiString() {
		return subject.getUiString()+"("+classroomSession.getUiString()+")";
	}

	public static final String FIELD_CLASSROOMSESSION = "classroomSession";
	public static final String FIELD_SUBJECT = "subject";
	public static final String FIELD_TEACHER = "teacher";

	public static final String COLUMN_CLASSROOMSESSION = "classroomSession";
	public static final String COLUMN_SUBJECT = "subject";
	public static final String COLUMN_TEACHER = "teacher";
	
}
