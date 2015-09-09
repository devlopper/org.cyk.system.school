package org.cyk.system.school.model.actor;

import java.io.Serializable;

import javax.persistence.Entity;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.party.person.AbstractActor;

@Getter @Setter @Entity
public class Teacher extends AbstractActor implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	
}
