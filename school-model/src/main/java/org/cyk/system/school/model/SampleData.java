package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.root.model.AbstractSampleData;
import org.cyk.system.school.model.actor.StudentReportTemplateFile;
import org.cyk.utility.common.generator.RandomDataProvider;

public class SampleData extends AbstractSampleData implements Serializable {

	private static final long serialVersionUID = -1887987316565799879L;
	
	public static Collection<StudentReportTemplateFile> createStudentReportTemplateFiles(){
		Collection<StudentReportTemplateFile> collection = RandomDataProvider.generate(StudentReportTemplateFile.class, 1);
		//StudentReportTemplateFile report = collection.iterator().next();
		
		return collection;
	}
	
	
	public static void main(String[] args) {
		System.out.println("SampleData.main()");
	}
	
}
