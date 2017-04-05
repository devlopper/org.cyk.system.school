package org.cyk.system.school.model.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectReport;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;

@Getter @Setter @NoArgsConstructor
public class StudentClassroomSessionDivisionSubjectReport extends AbstractStudentNodeReport<StudentClassroomSessionDivisionSubjectReport> implements Serializable {

	private static final long serialVersionUID = 7672451415743549818L;

	private StudentClassroomSessionDivisionReport studentClassroomSessionDivision;
	private ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject;
	
	public StudentClassroomSessionDivisionSubjectReport(StudentClassroomSessionDivisionReport studentClassroomSessionDivision,ClassroomSessionDivisionSubjectReport classroomSessionDivisionSubject
			,StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject) {
		super();
		this.studentClassroomSessionDivision = studentClassroomSessionDivision;
		this.classroomSessionDivisionSubject = classroomSessionDivisionSubject;
		setSource(studentClassroomSessionDivisionSubject);
	}
	
	@Override
	public void setSource(Object source) {
		super.setSource(source);
		if(source!=null){
			
		}
	}
	
	@Override
	public String toString() {
		return classroomSessionDivisionSubject.getName()+" | "+marks;
	}
}