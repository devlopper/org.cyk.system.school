package org.cyk.system.school.model.session;

import java.io.Serializable;

import org.cyk.system.root.model.file.report.AbstractReportTemplateFile;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionReportTemplateFile extends AbstractReportTemplateFile<StudentClassroomSessionDivisionReportTemplateFile> implements Serializable {

	private static final long serialVersionUID = -6025941646465245555L;
	
	private StudentClassroomSessionDivisionReport studentClassroomSessionDivision = new StudentClassroomSessionDivisionReport();
	
	@Override
	public void generate() {
		super.generate();
		studentClassroomSessionDivision.generate();
		StudentClassroomSessionDivisionReport previousStudentClassroomSessionDivision = new StudentClassroomSessionDivisionReport();
		previousStudentClassroomSessionDivision.generate();
		studentClassroomSessionDivision.setPrevious(previousStudentClassroomSessionDivision);
		previousStudentClassroomSessionDivision.setNext(studentClassroomSessionDivision);
	}
	
}
