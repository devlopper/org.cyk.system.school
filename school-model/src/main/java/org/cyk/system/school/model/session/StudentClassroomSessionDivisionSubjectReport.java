package org.cyk.system.school.model.session;

import java.io.Serializable;

import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionSubjectReport extends AbstractStudentNodeReport<StudentClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	private StudentClassroomSessionDivisionReportTemplateFile studentClassroomSessionDivision;
	private ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject;
	
	public StudentClassroomSessionDivisionSubjectReport(StudentClassroomSessionDivisionReportTemplateFile studentClassroomSessionDivision,ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject) {
		super();
		this.studentClassroomSessionDivision = studentClassroomSessionDivision;
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
	}
	
	@Override
	public String toString() {
		return classroomSessionDivisionSubject.getName()+" | "+marks;
	}
}