package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryOneFormModel.FIELD_IDENTIFIABLE,type=ClassroomSession.class)})
public class ClassroomSessionQueryOneFormModel extends AbstractClassroomSessionQueryOneFormModel<ClassroomSession> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
		
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionSelectOnePageAdapter<ClassroomSession> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSession.class);
		}
		
		protected void initialiseSelect(AbstractSelectOnePage<?> selectPage){
			super.initialiseSelect(selectPage);
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, AbstractQueryOneFormModel.FIELD_IDENTIFIABLE, null,null,null);
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionConsultClassroomSessionDivisionBroadsheet().equals(actionIdentifier)){
				ClassroomSessionDivision classroomSessionDivision = inject(ClassroomSessionDivisionBusiness.class).findByClassroomSessionByOrderNumber(
						((ClassroomSessionQueryOneFormModel)data).getIdentifiable(), inject(AcademicSessionBusiness.class).findCurrent(null).getNodeInformations().getCurrentClassroomSessionDivisionIndex());
				WebNavigationManager.getInstance().redirectTo(SchoolWebManager.getInstance().getOutcomeConsultClassroomSessionDivisionBroadsheet(),new Object[]{
					UniformResourceLocatorParameter.IDENTIFIABLE,classroomSessionDivision
				}); 
			}
		}
	}
}