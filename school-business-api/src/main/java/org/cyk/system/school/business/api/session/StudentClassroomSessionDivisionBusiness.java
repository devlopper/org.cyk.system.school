package org.cyk.system.school.business.api.session;

import java.util.Collection;

import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.school.business.api.subject.AbstractStudentResultsBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentSubject;

public interface StudentClassroomSessionDivisionBusiness extends AbstractStudentResultsBusiness<ClassroomSessionDivision,StudentClassroomSessionDivision,StudentSubject> {

	void buildReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	void buildReport(Collection<ClassroomSessionDivision> classroomSessionDivisions);
	
	ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> findReport(Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions);
	
	Collection<StudentClassroomSessionDivision> findByClassroomSessionDivision(ClassroomSessionDivision classroomSessionDivision);
	StudentClassroomSessionDivision findByStudentByClassroomSessionDivision(Student student, ClassroomSessionDivision classroomSessionDivision);
	
	
}
