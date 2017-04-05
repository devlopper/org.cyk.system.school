package org.cyk.system.school.model.subject;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;
import org.cyk.system.root.model.file.report.LabelValueCollectionReport;
import org.cyk.system.school.model.NodeResultsReport;

@Getter @Setter
public abstract class AbstractNodeReport<NODE> extends AbstractIdentifiableReport<NODE> implements Serializable {

	private static final long serialVersionUID = -4651687386219470908L;

	protected NodeResultsReport results = new NodeResultsReport();
	protected String numberOfStudents,highestAverage,average,lowestAverage,openedTime;
	
	protected LabelValueCollectionReport labelValueCollection = new LabelValueCollectionReport();
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			
		}
	}
	
	@Override
	public void generate() {
		super.generate();
		numberOfStudents = provider.randomPositiveInt(20)+"";
		average = provider.randomPositiveInt(20)+"";
		highestAverage = provider.randomPositiveInt(20)+"";
		lowestAverage = provider.randomPositiveInt(20)+"";
	}
	
}
