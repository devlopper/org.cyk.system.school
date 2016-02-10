package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.model.AbstractQueryFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.SelectPage;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivision.class)})
public class ClassroomSessionDivisionQueryFormModel extends AbstractClassroomSessionDivisionQueryFormModel<ClassroomSessionDivision> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Override @Sequence(direction=Direction.AFTER,field=FIELD_CLASSROOM_SESSION)
	public ClassroomSessionDivision getIdentifiable() {
		return super.getIdentifiable();
	}
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionDivisionSelectPageAdapter<ClassroomSessionDivision> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivision.class);
		}
		
		protected void initialiseSelect(SelectPage selectPage){
			super.initialiseSelect(selectPage);
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, FIELD_CLASSROOM_SESSION, AbstractQueryFormModel.FIELD_IDENTIFIABLE,null,null);
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectTo(SchoolWebManager.getInstance().getOutcomeUpdateStudentClassroomSessionDivisionResults()
						,new Object[]{UIManager.getInstance().getIdentifiableParameter(),((ClassroomSessionDivisionQueryFormModel)data).getIdentifiable().getIdentifier()}); 
			}
		}
	}
}