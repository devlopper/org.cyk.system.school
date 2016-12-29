package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.CascadeType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;
import javax.persistence.OneToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @MappedSuperclass @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public abstract class AbstractStudentResult<LEVEL,DETAIL> extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_STUDENT) @NotNull protected Student student;
	
	@OneToOne(cascade={CascadeType.PERSIST,CascadeType.MERGE}) protected StudentResults results;
	
	@Transient protected Collection<DETAIL> details = new ArrayList<>();//TODO to be removed to use only detailCollection
	
	@Transient protected IdentifiableRuntimeCollection<DETAIL> detailCollection = new IdentifiableRuntimeCollection<>();
	
	{
		cascadeOperationToChildren = Boolean.TRUE;
		cascadeOperationToMaster = Boolean.FALSE;
	}
	
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

	/**/
	
	public static final String COLUMN_STUDENT = "student";
}
