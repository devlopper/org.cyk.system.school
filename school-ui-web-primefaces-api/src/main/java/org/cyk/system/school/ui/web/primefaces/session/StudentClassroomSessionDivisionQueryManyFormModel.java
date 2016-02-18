package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;
import org.cyk.utility.common.FileExtension;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter
public class StudentClassroomSessionDivisionQueryManyFormModel extends AbstractQueryManyFormModel.Default<StudentClassroomSessionDivision> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractSelectManyPage.Listener.Adapter.Default<StudentClassroomSessionDivision,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(StudentClassroomSessionDivision.class);
		}
		
		@Override
		public Collection<StudentClassroomSessionDivision> getIdentifiables(AbstractSelectManyPage<?> selectManyPage) {
			return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness().findByClassroomSessionDivisionIndex(
					SchoolBusinessLayer.getInstance().getAcademicSessionBusiness().findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			WebNavigationManager.getInstance().redirectToFileConsultManyPage(SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionBusiness()
					.findReportFiles(((StudentClassroomSessionDivisionQueryManyFormModel)data).getIdentifiables()), FileExtension.PDF);
		}
		
	}
}