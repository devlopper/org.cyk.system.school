package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.util.List;

import org.cyk.system.root.model.file.report.AbstractIdentifiableReport;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public abstract class AbstractNodeReport<NODE> extends AbstractIdentifiableReport<NODE> implements Serializable {

	private static final long serialVersionUID = -4651687386219470908L;

	protected String numberOfStudents,highestAverage,average,lowestAverage,openedTime;
	
	
	@Override
	public void generate() {
		globalIdentifier.setName((String) provider.randomFromList(getRandomNames()));
		numberOfStudents = provider.randomPositiveInt(20)+"";
		average = provider.randomPositiveInt(20)+"";
		highestAverage = provider.randomPositiveInt(20)+"";
		lowestAverage = provider.randomPositiveInt(20)+"";
	}
	
	protected List<String> getRandomNames(){
		return null;
	}

}
