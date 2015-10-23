package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(crudStrategy=CrudStrategy.BUSINESS)
public class SubjectEvaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@JoinColumn(name="subjectEvaluationType") @ManyToOne @NotNull private SubjectEvaluationType type;
	@Temporal(TemporalType.TIMESTAMP) @Column(name="happeningDate") @NotNull private Date date;
	@NotNull private Boolean coefficientApplied = Boolean.TRUE;
	
	/**/
	
	@Transient private Collection<StudentSubjectEvaluation> studentSubjectEvaluations;

	public SubjectEvaluation(SubjectEvaluationType type,Boolean coefficientApplied) {
		super();
		this.type = type;
		this.coefficientApplied = coefficientApplied;
	}
	
	public Collection<StudentSubjectEvaluation> getStudentSubjectEvaluations(){
		if(studentSubjectEvaluations==null)
			studentSubjectEvaluations = new ArrayList<>();
		return studentSubjectEvaluations;
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_DATE = "date";
	
}
