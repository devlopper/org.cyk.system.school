package org.cyk.system.school.model.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionDivisionReport extends AbstractGeneratable<ClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private String name,numberOfStudents,average,highestAverage,lowestAverage;
	
	@Override
	public void generate() {
		name = provider.randomWord(5, 10);
		numberOfStudents = provider.randomPositiveInt(20)+"";
		average = provider.randomPositiveInt(20)+"";
		highestAverage = provider.randomPositiveInt(20)+"";
		lowestAverage = provider.randomPositiveInt(20)+"";
	}
	
}
