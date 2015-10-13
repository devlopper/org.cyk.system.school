package org.cyk.system.school.business.api.session;

import org.cyk.system.company.business.api.CompanyReportProducer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;

public interface SchoolReportProducer extends CompanyReportProducer {

	StudentClassroomSessionDivisionReport produceStudentClassroomSessionDivisionReport(StudentClassroomSessionDivision studentClassroomSessionDivision);
	
}
