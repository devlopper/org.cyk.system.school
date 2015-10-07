package org.cyk.system.school.model.subject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import org.cyk.utility.common.generator.AbstractGeneratable;

@Getter @Setter
public class ClassroomSessionDivisionSubjectReport extends AbstractGeneratable<ClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = -4651687386219470908L;

	private String name,numberOfStudents,coefficient,highestAverage,average;
	
	@Override
	public void generate() {
		name = (String) provider.randomFromList(RANDOM_NAMES);
		numberOfStudents = provider.randomPositiveInt(50)+"";
		coefficient = positiveFloatNumber(999, 0, 99);
		highestAverage = positiveFloatNumber(999, 0, 99);
		average = positiveFloatNumber(999, 0, 99);
	}
	
	public static final List<String> RANDOM_NAMES = new ArrayList<>();
	static{
		RANDOM_NAMES.add("Mathematics");
		RANDOM_NAMES.add("Grammar");
		RANDOM_NAMES.add("Reading & Comprehensive");
		RANDOM_NAMES.add("Handwriting");
		RANDOM_NAMES.add("Spelling");
		RANDOM_NAMES.add("Phonics");
		RANDOM_NAMES.add("Creative writing");
		RANDOM_NAMES.add("Moral education");
		RANDOM_NAMES.add("Social studies");
		RANDOM_NAMES.add("Science");
		RANDOM_NAMES.add("French");
		RANDOM_NAMES.add("Art & Craft");
		RANDOM_NAMES.add("Music");
		RANDOM_NAMES.add("ICT(Computer)");
		RANDOM_NAMES.add("Physical education");
	}

}
