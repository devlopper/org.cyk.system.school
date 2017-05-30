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
	private String subjectsBlockTitle,commentsBlockTitle,schoolStampBlockTitle;
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			
		}
	}
	
	@Override
	public void generate() {
		super.generate();
		subjectsBlockTitle = "COGNITIVE ASSESSMENT";
		commentsBlockTitle = "CLASS TEACHER COMMENTS AND SIGNATURE";
		schoolStampBlockTitle = "SCHOOL STAMP AND SIGNATURE";
		studentClassroomSessionDivision.generate();
		StudentClassroomSessionDivisionReport previousStudentClassroomSessionDivision = new StudentClassroomSessionDivisionReport();
		previousStudentClassroomSessionDivision.generate();
		studentClassroomSessionDivision.setPrevious(previousStudentClassroomSessionDivision);
		previousStudentClassroomSessionDivision.setNext(studentClassroomSessionDivision);
	}
	
}
