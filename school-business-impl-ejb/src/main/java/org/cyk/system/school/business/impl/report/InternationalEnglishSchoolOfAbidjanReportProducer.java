package org.cyk.system.school.business.impl.report;

import java.io.Serializable;

import org.cyk.system.root.business.api.TypedBusiness.CreateReportFileArguments;
import org.cyk.system.school.business.impl.AbstractSchoolReportProducer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReportTemplateFile;


public  class InternationalEnglishSchoolOfAbidjanReportProducer extends AbstractSchoolReportProducer.Default implements Serializable{
	private static final long serialVersionUID = 1L;
	
	@Override
	public StudentClassroomSessionDivisionReportTemplateFile produceStudentClassroomSessionDivisionReport(
			StudentClassroomSessionDivision studentClassroomSessionDivision,
			CreateReportFileArguments<StudentClassroomSessionDivision> arguments) {
		StudentClassroomSessionDivisionReportTemplateFile studentClassroomSessionDivisionReportTemplateFile = super.produceStudentClassroomSessionDivisionReport(studentClassroomSessionDivision, arguments);
		
		//TODO is there a possibility to make from Script ???
		studentClassroomSessionDivisionReportTemplateFile.getStudentClassroomSessionDivision().getClassroomSessionDivision().getClassroomSession().getAcademicSession()
			.getExistencePeriod().setFromYearToYear(studentClassroomSessionDivisionReportTemplateFile.getStudentClassroomSessionDivision().getClassroomSessionDivision().getClassroomSession().getAcademicSession()
			.getExistencePeriod().getFromYearToYear()+" ACADEMIC SESSION");
		
		return studentClassroomSessionDivisionReportTemplateFile;
	}
}