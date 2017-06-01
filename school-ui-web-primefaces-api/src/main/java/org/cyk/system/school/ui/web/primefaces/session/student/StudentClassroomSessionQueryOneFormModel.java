package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.util.Collection;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.StudentClassroomSessionBusiness;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;

@Getter @Setter
public class StudentClassroomSessionQueryOneFormModel extends AbstractQueryOneFormModel.Default<StudentClassroomSession> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractSelectManyPage.Listener.Adapter.Default<StudentClassroomSession,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(StudentClassroomSession.class);
		}
		
		@Override
		public Collection<StudentClassroomSession> getIdentifiables(AbstractSelectManyPage<?> selectManyPage) {
			return inject(StudentClassroomSessionBusiness.class).findByAcademicSession(inject(AcademicSessionBusiness.class).findDefaultedSchoolDefaulted());
		}
		
		@Override
		public void serve(AbstractSelectManyPage<?> selectManyPage,Object data, String actionIdentifier) {
			
		}
		
	}
}