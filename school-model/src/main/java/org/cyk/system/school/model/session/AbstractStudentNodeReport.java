package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.mathematics.IntervalReport;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.StudentResultsReport;
import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter @NoArgsConstructor
public abstract class AbstractStudentNodeReport<NODE> extends AbstractIdentifiableReport<NODE> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	protected StudentResultsReport results = new StudentResultsReport();
	
	protected IntervalReport averageScale = new IntervalReport();
	
	protected List<String> marks = new ArrayList<>();
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			results.setSource(((AbstractStudentResult<?,?>)source).getResults());
		}
	}
	
	@Override
	public void generate() {
		results.generate();
		averageScale.generate();
		for(int i=0;i<3;i++)
			marks.add(positiveFloatNumber(999, 0, 99));
	}
	
	/**/
	protected static final StudentResultsReport NULL_STUDENT_RESULTS_REPORT = new StudentResultsReport();
	static {
		NULL_STUDENT_RESULTS_REPORT.getEvaluationSort().getAverage().setValue(String.valueOf(AbstractGeneratable.NULL_VALUE));
		NULL_STUDENT_RESULTS_REPORT.getEvaluationSort().getRank().setValue(String.valueOf(AbstractGeneratable.NULL_VALUE));
	}
	
}