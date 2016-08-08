package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;

import org.cyk.system.root.model.network.UniformResourceLocatorParameter;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.SchoolConstant;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.UIManager;
import org.cyk.ui.api.command.UICommandable.Parameter;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryOneFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivision.class)})
public class ClassroomSessionDivisionQueryOneFormModel extends AbstractClassroomSessionDivisionQueryOneFormModel<ClassroomSessionDivision> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Override @Sequence(direction=Direction.AFTER,field=FIELD_CLASSROOM_SESSION)
	public ClassroomSessionDivision getIdentifiable() {
		return super.getIdentifiable();
	}
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionDivisionSelectOnePageAdapter<ClassroomSessionDivision> implements Serializable {
		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivision.class);
		}
		
		protected void initialiseSelect(AbstractSelectOnePage<?> selectPage){
			super.initialiseSelect(selectPage);
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, FIELD_CLASSROOM_SESSION, AbstractQueryOneFormModel.FIELD_IDENTIFIABLE,null,null);
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionUpdateStudentClassroomSessionDivisionResults().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectToDynamicConsultOne(((ClassroomSessionDivisionQueryOneFormModel)data).getIdentifiable()
						,Arrays.asList(new Parameter(UniformResourceLocatorParameter.TAB_ID
						,UIManager.getInstance().businessEntityInfos(StudentClassroomSessionDivision.class).getUserInterface().getLabelId()))); 
			}else if(SchoolBusinessLayer.getInstance().getActionConsultClassroomSessionDivisionBroadsheet().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectToDynamicConsultOne(((ClassroomSessionDivisionQueryOneFormModel)data).getIdentifiable()
						,Arrays.asList(new Parameter(UniformResourceLocatorParameter.TAB_ID,SchoolConstant.UI_TAB_BROADSHEET_ID))); 
			}
		}
	}
}