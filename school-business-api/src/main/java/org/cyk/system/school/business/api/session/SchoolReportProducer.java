package org.cyk.system.school.business.api.session;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.company.business.api.CompanyReportProducer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;

public interface SchoolReportProducer extends CompanyReportProducer {

	StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision,
			StudentClassroomSessionDivisionReportParameters parameters);
	
	String getEvaluationTypeCode(StudentSubjectEvaluation studentSubjectEvaluation);
	
	/**/
	
	@Getter @Setter
	public static class StudentClassroomSessionDivisionReportParameters implements Serializable{
		private static final long serialVersionUID = 1446488904290411180L;
		private Set<String> evaluationTypeCodes=new LinkedHashSet<>();
		private Boolean sumMarks = Boolean.FALSE;
		
	}
	
	StudentClassroomSessionDivisionReportParameters DEFAULT_STUDENT_CLASSROOM_SESSION_DIVISION_REPORT_PARAMETERS = new StudentClassroomSessionDivisionReportParameters();
}
