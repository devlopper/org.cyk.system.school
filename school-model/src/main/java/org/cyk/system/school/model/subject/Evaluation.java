package org.cyk.system.school.model.subject;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.root.model.IdentifiableRuntimeCollection;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.utility.common.annotation.ModelBean;
import org.cyk.utility.common.annotation.ModelBean.CrudStrategy;
import org.cyk.utility.common.annotation.ModelBean.GenderType;

@Getter @Setter @Entity @NoArgsConstructor @ModelBean(genderType=GenderType.FEMALE,crudStrategy=CrudStrategy.BUSINESS)
public class Evaluation extends AbstractIdentifiable implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;

	@JoinColumn(name="type") @ManyToOne @NotNull private ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType;
	@NotNull private Boolean coefficientApplied = SchoolConstant.Configuration.Evaluation.COEFFICIENT_APPLIED;
	
	@Transient private IdentifiableRuntimeCollection<StudentClassroomSessionDivisionSubjectEvaluation> studentClassroomSessionDivisionSubjectEvaluations = new IdentifiableRuntimeCollection<>();

	public Evaluation(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType) {
		super();
		this.classroomSessionDivisionSubjectEvaluationType = classroomSessionDivisionSubjectEvaluationType;
	}
	
	@Override
	public Evaluation setCode(String code) {
		return (Evaluation) super.setCode(code);
	}
	
	@Override
	public String getUiString() {
		return classroomSessionDivisionSubjectEvaluationType.getUiString();
	}
	
	@Override
	public String toString() {
		return classroomSessionDivisionSubjectEvaluationType.toString();
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, classroomSessionDivisionSubjectEvaluationType.getIdentifier(),getBirthDate(),coefficientApplied);
	}
	private static final String LOG_FORMAT = Evaluation.class.getSimpleName()+"(TYPE=%s date=%s COEF_APPLIED=%s)";
	
	public static final String FIELD_TYPE = "classroomSessionDivisionSubjectEvaluationType";
	public static final String FIELD_DATE = "date";
	
}
