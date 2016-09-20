package org.cyk.system.school.model.actor;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.party.person.AbstractActorReport;

@Getter @Setter
public class StudentReport extends AbstractActorReport<StudentReport> implements Serializable {

	private static final long serialVersionUID = 7967195187341455422L;
	
}
