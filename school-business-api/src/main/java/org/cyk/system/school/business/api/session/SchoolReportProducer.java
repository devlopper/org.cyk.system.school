package org.cyk.system.school.business.api.session;

import org.cyk.system.company.business.api.CompanyReportProducer;
import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;

public interface SchoolReportProducer extends CompanyReportProducer {

	StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision
			,CreateReportFileArguments<StudentClassroomSessionDivision> createReportFileArguments);
	
	String getEvaluationTypeCode(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation);
	
	/**/
	
}
