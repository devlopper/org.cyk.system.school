package org.cyk.system.school.model.subject;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter
public class ClassroomSessionDivisionSubjectReport extends AbstractGeneratable<ClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = -4651687386219470908L;

	private String numberOfStudents,highestAverage,average;
	
	@Override
	public void generate() {
		numberOfStudents = provider.randomPositiveInt(50)+"";
		highestAverage = positiveFloatNumber(20, 0, 99);
		average = positiveFloatNumber(20, 0, 99);
	}

}
