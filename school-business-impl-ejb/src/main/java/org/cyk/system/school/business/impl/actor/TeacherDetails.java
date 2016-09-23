package org.cyk.system.school.business.impl.actor;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.impl.party.person.AbstractActorDetails;
import org.cyk.system.school.model.actor.Teacher;

@Getter @Setter
public class TeacherDetails extends AbstractActorDetails.AbstractDefault<Teacher> implements Serializable {

	private static final long serialVersionUID = 1L;

	public TeacherDetails(Teacher teacher) {
		super(teacher);
	}

}
