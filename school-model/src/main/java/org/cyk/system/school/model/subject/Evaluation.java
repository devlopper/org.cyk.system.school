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
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(genderType=GenderType.FEMALE,crudStrategy=CrudStrategy.BUSINESS)
public class Evaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@JoinColumn(name="type") @ManyToOne @NotNull private ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType;
	@Temporal(TemporalType.TIMESTAMP) @Column(name="happeningDate") @NotNull private Date date;
	@NotNull private Boolean coefficientApplied = Boolean.FALSE;
	
	/**/
	
	@Transient private Collection<StudentSubjectEvaluation> studentSubjectEvaluations;

	public Evaluation(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType,Boolean coefficientApplied) {
		super();
		this.classroomSessionDivisionSubjectEvaluationType = classroomSessionDivisionSubjectEvaluationType;
		this.coefficientApplied = coefficientApplied;
	}
	
	public Collection<StudentSubjectEvaluation> getStudentSubjectEvaluations(){
		if(studentSubjectEvaluations==null)
			studentSubjectEvaluations = new ArrayList<>();
		return studentSubjectEvaluations;
	}
	
	@Override
	public String getUiString() {
		return classroomSessionDivisionSubjectEvaluationType.getUiString();
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, classroomSessionDivisionSubjectEvaluationType.getIdentifier(),date,coefficientApplied);
	}
	private static final String LOG_FORMAT = Evaluation.class.getSimpleName()+"(TYPE=%s date=%s COEF_APPLIED=%s)";
	
	public static final String FIELD_TYPE = "classroomSessionDivisionSubjectEvaluationType";
	public static final String FIELD_DATE = "date";
	
}
