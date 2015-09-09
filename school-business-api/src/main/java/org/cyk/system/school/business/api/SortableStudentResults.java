package org.cyk.system.school.business.api;

import java.io.Serializable;
import java.math.BigDecimal;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.mathematics.Sortable;
import org.cyk.system.root.model.mathematics.Rank;
import org.cyk.system.root.model.mathematics.Sort;
import org.cyk.system.school.model.AbstractStudentResult;

@Getter @Setter @AllArgsConstructor
public class SortableStudentResults implements Sortable, Serializable {

	private static final long serialVersionUID = -1800577155978523663L;

	private AbstractStudentResult<?, ?> studentResult;
	private Boolean evaluation;
	
	@Override
	public Rank getRank() {
		return sort().getRank();
	}

	@Override
	public BigDecimal getValue() {
		return sort().getAverage().getValue();
	}
	
	private Sort sort(){
		if(evaluation==null || Boolean.TRUE.equals(evaluation))
			return studentResult.getResults().getEvaluationSort();
		return studentResult.getResults().getLectureAttendanceSort();
	}

}
