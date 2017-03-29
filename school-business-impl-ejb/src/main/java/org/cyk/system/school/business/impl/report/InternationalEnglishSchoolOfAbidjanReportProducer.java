package org.cyk.system.school.business.impl.report;

import java.io.Serializable;

import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;
import org.joda.time.DateTime;


public  class InternationalEnglishSchoolOfAbidjanReportProducer extends AbstractSchoolReportProducer.Default implements Serializable{
	private static final long serialVersionUID = 1L;
	
	@Override
	public StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(
			StudentClassroomSessionDivision studentClassroomSessionDivision,
			CreateReportFileArguments<StudentClassroomSessionDivision> arguments) {
		StudentClassroomSessionDivisionReportTemplateFile studentClassroomSessionDivisionReportTemplateFile = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision, arguments);
		int y1 = new DateTime(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getExistencePeriod().getFromDate()).getYear();
		int y2 = new DateTime(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession().getAcademicSession().getExistencePeriod().getToDate()).getYear();
		studentClassroomSessionDivisionReportTemplateFile.getStudentClassroomSessionDivision().getAcademicSession()
			.setFromDateToDate(y1+"/"+y2+" ACADEMIC SESSION");
		return studentClassroomSessionDivisionReportTemplateFile;
	}
}