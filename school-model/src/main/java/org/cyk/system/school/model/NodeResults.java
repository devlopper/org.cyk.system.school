package org.cyk.system.school.model;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Embeddable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.AbstractModelElement;

@Getter @Setter @Embeddable @NoArgsConstructor
public class NodeResults extends AbstractModelElement implements Serializable {

	private static final long serialVersionUID = 2742833783679362737L;
	
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal average;
	
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal averageHighest;
	
	@Column(precision=5,scale=FLOAT_SCALE) private BigDecimal averageLowest;
	
	private Integer numberOfStudent;
	
	private Integer numberOfStudentPassingEvaluationAverage;
	
	@Override
	public String getUiString() {
		return average+" ";
	}
	
	@Override
	public String getLogMessage() {
		return String.format(LOG_FORMAT, average,averageHighest,averageLowest,numberOfStudent,numberOfStudentPassingEvaluationAverage);
	}
	private static final String LOG_FORMAT = NodeResults.class.getSimpleName()+"(A=%s H=%s L=%s STUD=%s,PA=%s)";

	public static final String FIELD_NUMBER_OF_STUDENT = "numberOfStudent";
}
