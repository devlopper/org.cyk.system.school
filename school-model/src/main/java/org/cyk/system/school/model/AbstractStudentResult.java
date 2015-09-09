package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Embedded;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.actor.Student;

@Getter @Setter @MappedSuperclass
public abstract class AbstractStudentResult<LEVEL,DETAIL> extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne protected Student student;
	
	@Embedded protected StudentResults results = new StudentResults();
	
	@Transient protected Collection<DETAIL> details = new ArrayList<>();
	
	public abstract LEVEL getLevel();

}
