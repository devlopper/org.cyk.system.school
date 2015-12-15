package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.model.AbstractQueryFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.SelectPage;
import org.cyk.ui.web.primefaces.page.SelectPageListener;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;
import org.cyk.utility.common.cdi.AbstractBean;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivision.class)})
public class ClassroomSessionDivisionQueryFormModel extends AbstractQueryFormModel.Default<ClassroomSessionDivision> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
	
	@Override @Sequence(direction=Direction.AFTER,field=CLASSROOM_SESSION)
	public ClassroomSessionDivision getIdentifiable() {
		return super.getIdentifiable();
	}
	
	public static final String CLASSROOM_SESSION = "classroomSession";
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends SelectPageListener.Adapter.Default<ClassroomSessionDivision,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivision.class);
		}
		
		@Override
		public void afterInitialisationEnded(AbstractBean bean) {
			super.afterInitialisationEnded(bean);
			final SelectPage selectPage = (SelectPage) bean;
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, CLASSROOM_SESSION
					, AbstractQueryFormModel.FIELD_IDENTIFIABLE,null,null);
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