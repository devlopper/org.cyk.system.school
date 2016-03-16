package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.session.StudentClassroomSessionDivisionBusiness.BuildReportArguments;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.PrimefacesManager;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.FileExtension;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.computation.ExecutionProgress;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryManyFormModel.FIELD_IDENTIFIABLES,type=ClassroomSession.class)})
public class ClassroomSessionQueryManyFormModel extends AbstractClassroomSessionQueryManyFormModel<ClassroomSession> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionSelectManyPageAdapter<ClassroomSession> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSession.class);
		}
		
		@Override
		protected void initialiseSelect(AbstractSelectManyPage<?> selectPage) {
			super.initialiseSelect(selectPage);
			selectPage.getForm().getSubmitCommandable().getCommand().setShowExecutionProgress(Boolean.TRUE);
			selectPage.setExecutionProgress(new ExecutionProgress("Build Student Classroom Session Division Report",null));
			selectPage.getForm().getSubmitCommandable().getCommand().setExecutionProgress(selectPage.getExecutionProgress());
			PrimefacesManager.getInstance().configureProgressBar(selectPage.getForm().getSubmitCommandable());
		}
		
		@Override
		public Collection<ClassroomSession> getIdentifiables(AbstractSelectManyPage<?> selectManyPage) {
			return SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().findAll();
		}
		
		@Override
		public void serve(AbstractSelectManyPage<?> selectManyPage,Object data, String actionIdentifier) {
			Collection<ClassroomSession> classroomSessions = ((ClassroomSessionQueryManyFormModel)data).getIdentifiables();
			if(SchoolBusinessLayer.getInstance().getActionConsultStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().findByClassroomSessions(classroomSessions);
				Collection<StudentClassroomSessionDivision> studentClassroomSessionDivisions = SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivisions(classroomSessionDivisions);
				SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findReportFiles(studentClassroomSessionDivisions);
				WebNavigationManager.getInstance().redirectToFileConsultManyPage(SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findReportFiles(studentClassroomSessionDivisions), FileExtension.PDF);
			}else if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionReportFiles().equals(actionIdentifier)){
				Collection<ClassroomSessionDivision> classroomSessionDivisions = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness()
						.findByClassroomSessionsByIndex(classroomSessions,SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				BuildReportArguments buildReportArguments = new BuildReportArguments();
				buildReportArguments.setExecutionProgress(selectManyPage.getExecutionProgress());
				SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().buildReport(classroomSessionDivisions,buildReportArguments);
			}
		}
	}
}