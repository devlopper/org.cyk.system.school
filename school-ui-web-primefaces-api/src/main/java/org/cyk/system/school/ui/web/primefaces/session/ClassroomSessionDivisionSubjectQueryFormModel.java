package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryOneFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivisionSubject.class)})
public class ClassroomSessionDivisionSubjectQueryFormModel extends AbstractClassroomSessionDivisionSubjectQueryFormModel<ClassroomSessionDivisionSubject> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Override @Sequence(direction=Direction.AFTER,field=FIELD_CLASSROOM_SESSION_DIVISION)
	public ClassroomSessionDivisionSubject getIdentifiable() {
		return super.getIdentifiable();
	}
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionDivisionSubjectPageAdapter<ClassroomSessionDivisionSubject> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivisionSubject.class);
		}
		
		@Override
		protected void initialiseSelect(AbstractSelectOnePage<?> selectPage) {
			super.initialiseSelect(selectPage);
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, FIELD_CLASSROOM_SESSION, FIELD_CLASSROOM_SESSION_DIVISION
					, AbstractQueryOneFormModel.FIELD_IDENTIFIABLE,null);
		}
		/*
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectToDynamicCreate(((ClassroomSessionDivisionSubjectQueryFormModel)data).getSubjectEvaluationType()
						, Evaluation.class,null); 
			}
		}*/
	}
}