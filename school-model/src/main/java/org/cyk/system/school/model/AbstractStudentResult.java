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
	
	@OneToOne(cascade={CascadeType.PERSIST,CascadeType.MERGE}) protected StudentResults results;
	
	@Transient protected Collection<DETAIL> details = new ArrayList<>();
	
	@Transient protected Boolean cascadeTopDownOnCreate = Boolean.TRUE;
	@Transient protected Boolean cascadeBottomUpOnCreate = Boolean.FALSE;
	
	public abstract LEVEL getLevel();
	
	@Override
	public String toString() {
		return "("+identifier+","+student.getIdentifier()+","+((AbstractIdentifiable)getLevel()).getIdentifier()+")";
	}
	
	@Override
	public String getUiString() {
		return student.getUiString();
	}
	
	
	
	public static final String FIELD_STUDENT = "student";
	public static final String FIELD_RESULTS = "results";

}
