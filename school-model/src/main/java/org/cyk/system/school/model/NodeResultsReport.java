package org.cyk.system.school.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter
public class NodeResultsReport extends AbstractGeneratable<NodeResultsReport> implements Serializable {

	private static final long serialVersionUID = 1L;

	private String average,averageHighest,averageLowest,numberOfStudent,numberOfStudentPassingEvaluationAverage,numberOfStudentNotPassingEvaluationAverage;
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			this.average = format(((NodeResults)source).getAverage());
			this.averageHighest = format(((NodeResults)source).getAverageHighest());
			this.averageLowest = format(((NodeResults)source).getAverageLowest());
			this.numberOfStudent = format(((NodeResults)source).getNumberOfStudent());
			this.numberOfStudentPassingEvaluationAverage = format(((NodeResults)source).getNumberOfStudentPassingEvaluationAverage());
			this.numberOfStudentNotPassingEvaluationAverage = format(((NodeResults)source).getNumberOfStudentNotPassingEvaluationAverage());
		}
	}
	
	@Override
	public void generate() {
		this.average = provider.randomInt(0, 100)+"";
		this.averageHighest = provider.randomInt(0, 100)+"";
		this.averageLowest = provider.randomInt(0, 100)+"";
		this.numberOfStudent = provider.randomInt(0, 100)+"";
		this.numberOfStudentPassingEvaluationAverage = provider.randomInt(0, 100)+"";
		this.numberOfStudentNotPassingEvaluationAverage = provider.randomInt(0, 100)+"";
	}

}
