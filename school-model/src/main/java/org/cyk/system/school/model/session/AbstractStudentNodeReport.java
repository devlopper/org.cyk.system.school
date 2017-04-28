package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.mathematics.IntervalReport;
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.system.school.model.AbstractStudentResult;
import org.cyk.system.school.model.StudentResultsReport;
import org.cyk.utility.common.generator.AbstractGeneratable;
import org.cyk.utility.common.generator.RandomDataProvider;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public abstract class AbstractStudentNodeReport<NODE> extends AbstractIdentifiableReport<NODE> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	@Deprecated protected String average,averageCoefficiented,rank;
	protected StudentResultsReport results = new StudentResultsReport();
	protected IntervalReport averageScale = new IntervalReport();
	
	protected List<String> marks = new ArrayList<>();
	@Deprecated protected ActorReport teacher = new ActorReport();//TODO to be deleted , it is not its place
	
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
		average = positiveFloatNumber(999, 0, 99);
		averageCoefficiented = positiveFloatNumber(999, 0, 99);
		rank = RandomDataProvider.getInstance().randomInt(1, 100)+"th";
		averageScale.generate();
		for(int i=0;i<3;i++)
			marks.add(positiveFloatNumber(999, 0, 99));
		teacher.generate();
	}
	
	/**/
	protected static final StudentResultsReport NULL_STUDENT_RESULTS_REPORT = new StudentResultsReport();
	static {
		NULL_STUDENT_RESULTS_REPORT.getEvaluationSort().getAverage().setValue(String.valueOf(AbstractGeneratable.NULL_VALUE));
		NULL_STUDENT_RESULTS_REPORT.getEvaluationSort().getRank().setValue(String.valueOf(AbstractGeneratable.NULL_VALUE));
	}
	
}