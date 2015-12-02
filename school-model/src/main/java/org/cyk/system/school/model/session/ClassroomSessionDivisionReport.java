package org.cyk.system.school.model.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.cyk.utility.common.generator.AbstractGeneratable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class ClassroomSessionDivisionReport extends AbstractGeneratable<ClassroomSessionDivisionReport> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private ClassroomSessionReport classroomSession = new ClassroomSessionReport();
	private String name,numberOfStudents,average,highestAverage,lowestAverage,openedTime;
	
	
	@Override
	public void generate() {
		classroomSession.generate();
		name = (String) provider.randomFromList(RANDOM_NAMES);
		numberOfStudents = provider.randomPositiveInt(20)+"";
		average = provider.randomPositiveInt(20)+"";
		highestAverage = provider.randomPositiveInt(20)+"";
		lowestAverage = provider.randomPositiveInt(20)+"";
	}
	
	/**/
	
	public static final List<String> RANDOM_NAMES = new ArrayList<>();
	static{
		for(String order : new String[]{"First","Second","Third"})
			for(String level : new String[]{"Primary","Secondary"})
				RANDOM_NAMES.add(order+" Term "+level+" Report Card");
	}
	
}
