package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.CascadeType;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;
import javax.persistence.OneToOne;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @MappedSuperclass @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public abstract class AbstractStudentResult<LEVEL,DETAIL> extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne protected Student student;
	
	@OneToOne(cascade={CascadeType.PERSIST,CascadeType.MERGE},orphanRemoval=true) protected StudentResults results;
	
	@Transient protected Collection<DETAIL> details = new ArrayList<>();
	
	public abstract LEVEL getLevel();
	
	public static final String FIELD_STUDENT = "student";

}
