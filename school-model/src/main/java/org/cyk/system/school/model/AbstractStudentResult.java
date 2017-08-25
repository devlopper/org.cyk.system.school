package org.cyk.system.school.model;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;
import javax.persistence.OneToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.root.model.search.AbstractFieldValueSearchCriteriaSet;
import org.cyk.system.school.model.actor.Student;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @MappedSuperclass @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public abstract class AbstractStudentResult<LEVEL,DETAIL> extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@ManyToOne @JoinColumn(name=COLUMN_STUDENT) @NotNull protected Student student;
	
	@OneToOne(cascade={CascadeType.PERSIST,CascadeType.MERGE}) @JoinColumn(name=COLUMN_RESULTS) protected StudentResults results; //TODO avoid using cascade
	
	@Transient protected IdentifiableRuntimeCollection<DETAIL> detailCollection = new IdentifiableRuntimeCollection<>();
	
	{
		cascadeOperationToChildren = Boolean.TRUE;
		cascadeOperationToMaster = Boolean.FALSE;
	}
	
	public abstract LEVEL getLevel();
	
	@Override
	public String toString() {
		return "("+identifier+","+student.getIdentifier()+","+((AbstractIdentifiable)getLevel()).getIdentifier()+","+results+")";
	}
	
	@Override
	public String getUiString() {
		return getCode(); //student.getUiString();
	}
	
	
	
	public static final String FIELD_STUDENT = "student";
	public static final String FIELD_RESULTS = "results";

	/**/
	
	public static final String COLUMN_STUDENT = "student";
	public static final String COLUMN_RESULTS = "results";
	
	/**/
	
	@Getter @Setter
	public static abstract class AbstractSearchCriteria extends AbstractFieldValueSearchCriteriaSet.AbstractIdentifiableSearchCriteriaSet implements Serializable {

		private static final long serialVersionUID = 6796076474234170332L;

		protected Student.SearchCriteria student = new Student.SearchCriteria();
		protected StudentResults.SearchCriteria results = new StudentResults.SearchCriteria();
		
		
	}
}
