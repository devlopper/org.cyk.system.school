package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.FieldOverride;
import org.cyk.utility.common.annotation.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryOneFormModel.FIELD_IDENTIFIABLE_FROM_COMBO,type=ClassroomSessionDivisionSubjectEvaluationType.class)})
public class ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel extends AbstractClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Override @Sequence(direction=Direction.AFTER,field=FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT)
	public ClassroomSessionDivisionSubjectEvaluationType getIdentifiableFromCombo() {
		return super.getIdentifiableFromCombo();
	}
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionDivisionSubjectEvaluationTypePageAdapter<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivisionSubjectEvaluationType.class);
			type = Type.IDENTIFIABLE_FROM_COMBO;
		}
		
		@Override
		protected void initialiseSelect(AbstractSelectOnePage<?> selectPage) {
			super.initialiseSelect(selectPage);
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, FIELD_CLASSROOM_SESSION, FIELD_CLASSROOM_SESSION_DIVISION
					,FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, AbstractQueryOneFormModel.FIELD_IDENTIFIABLE_FROM_COMBO);
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectToDynamicCreate(((ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel)data).getIdentifiableFromCombo()
						, Evaluation.class,null); 
			}
		}
	}
}