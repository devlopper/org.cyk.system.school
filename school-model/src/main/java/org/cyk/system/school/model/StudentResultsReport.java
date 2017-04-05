package org.cyk.system.school.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.mathematics.SortReport;

@Getter @Setter
public class StudentResultsReport extends AbstractIdentifiableReport<StudentResultsReport> implements Serializable {
	private static final long serialVersionUID = 6771208248128490701L;

	private SortReport evaluationSort = new SortReport();
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			evaluationSort.setSource( ((StudentResults)source).getEvaluationSort() );
		}
	}
	
	@Override
	public void generate() {
		super.generate();
		evaluationSort.generate();
	}
}
