package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.mathematics.IntervalReport;
import org.cyk.system.root.model.party.person.ActorReport;
import org.cyk.utility.common.generator.RandomDataProvider;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public abstract class AbstractStudentNodeReport<NODE> extends AbstractIdentifiableReport<NODE> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	protected String average,averageCoefficiented,rank;
	protected IntervalReport averageScale = new IntervalReport();
	
	protected List<String> marks = new ArrayList<>();
	protected ActorReport teacher = new ActorReport();//TODO to be deleted , it is not its place
	
	@Override
	public void generate() {
		average = positiveFloatNumber(999, 0, 99);
		averageCoefficiented = positiveFloatNumber(999, 0, 99);
		rank = RandomDataProvider.getInstance().randomInt(1, 100)+"th";
		averageScale.generate();
		for(int i=0;i<3;i++)
			marks.add(positiveFloatNumber(999, 0, 99));
		teacher.generate();
	}
	
}