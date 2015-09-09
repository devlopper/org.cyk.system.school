package org.cyk.system.school.business.impl;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.mathematics.ValueComparator;
import org.cyk.system.root.model.mathematics.Sort;
import org.cyk.system.school.business.api.SortableStudentResults;

public class SortableStudentResultsComparator extends ValueComparator<SortableStudentResults> implements Serializable {

	private static final long serialVersionUID = -2987218578033551664L;

	public SortableStudentResultsComparator(Boolean evaluation,Boolean ascending) {
		super(new StudentValueReader(evaluation),ascending);
	}
	
	public SortableStudentResultsComparator(Boolean evaluation) {
		this(evaluation,Boolean.FALSE);
	}
	
	@AllArgsConstructor @Getter @Setter
	private static class StudentValueReader implements ValueReader<SortableStudentResults>, Serializable {
		private static final long serialVersionUID = 5156971346892166775L;
		private Boolean evaluation;
		
		@Override
		public Object read(SortableStudentResults sortableStudentResults, Integer level) {
			Sort sort;
			if(evaluation==null || Boolean.TRUE.equals(evaluation))
				sort = sortableStudentResults.getStudentResult().getResults().getEvaluationSort();
			else
				sort = sortableStudentResults.getStudentResult().getResults().getLectureAttendanceSort();
			return sort.getAverage().getValue();
		}
	}

}
