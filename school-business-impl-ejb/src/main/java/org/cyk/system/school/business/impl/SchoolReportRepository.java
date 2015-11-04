package org.cyk.system.school.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import javax.inject.Singleton;

import lombok.Getter;

import org.cyk.system.root.business.impl.file.report.AbstractReportRepository;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFile;
import org.cyk.system.root.model.file.report.ReportBasedOnTemplateFileConfiguration;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivisionReport;
import org.cyk.utility.common.annotation.Deployment;
import org.cyk.utility.common.annotation.Deployment.InitialisationType;

@Singleton @Deployment(initialisationType=InitialisationType.EAGER,order=SchoolBusinessLayer.DEPLOYMENT_ORDER+1)
public class SchoolReportRepository extends AbstractReportRepository implements Serializable {

	private static final long serialVersionUID = 6917567891985885124L;

	@Getter private final String reportStudentClassroomSessionDivision = "scsd";
		
	@Override
	protected void initialisation() {
		super.initialisation();
		INSTANCE = this;
	}
	
	@Override
	public void build() {        	
		registerConfiguration(new ReportBasedOnTemplateFileConfiguration<StudentClassroomSessionDivision, ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport>>(reportStudentClassroomSessionDivision) {
			@SuppressWarnings("unchecked")
			@Override
			public <MODEL> ReportBasedOnTemplateFile<StudentClassroomSessionDivisionReport> build(Class<MODEL> arg0, Collection<MODEL> models, String arg2,Boolean arg3, Map<String, String[]> arg4) {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findReport((Collection<StudentClassroomSessionDivision>) models);
			}
		});
		
	}
	
	/**/
		
	private static SchoolReportRepository INSTANCE;
	public static SchoolReportRepository getInstance() {
		return INSTANCE;
	}
}
