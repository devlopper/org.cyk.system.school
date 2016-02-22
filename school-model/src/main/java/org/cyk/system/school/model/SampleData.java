package org.cyk.system.school.model;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.utility.common.generator.RandomDataProvider;

public class SampleData implements Serializable {

	private static final long serialVersionUID = -1887987316565799879L;

	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReports(){
		return RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
	}
	
	public static Collection<StudentClassroomSessionDivisionReport> createStudentClassroomSessionDivisionReports2(){
		return RandomDataProvider.generate(StudentClassroomSessionDivisionReport.class, 1);
	}
	
	public static void main(String[] args) {
		Collection<StudentClassroomSessionDivisionReport> reports = createStudentClassroomSessionDivisionReports();
		System.out.println("SampleData.main() : "+reports.iterator().next().getClassroomSessionDivisionSubjects().size());
	}
	
}
