package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.model.AbstractQueryFormModel;
import org.cyk.ui.web.api.WebNavigationManager;
import org.cyk.ui.web.primefaces.page.SelectPage;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverride;
import org.cyk.utility.common.annotation.user.interfaces.FieldOverrides;
import org.cyk.utility.common.annotation.user.interfaces.Sequence;
import org.cyk.utility.common.annotation.user.interfaces.Sequence.Direction;

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivisionSubjectEvaluationType.class)})
public class ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel extends AbstractClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Override @Sequence(direction=Direction.AFTER,field=FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT)
	public ClassroomSessionDivisionSubjectEvaluationType getIdentifiable() {
		return super.getIdentifiable();
	}
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends AbstractClassroomSessionDivisionSubjectEvaluationTypePageAdapter<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivisionSubjectEvaluationType.class);
		}
		
		@Override
		protected void initialiseSelect(SelectPage selectPage) {
			super.initialiseSelect(selectPage);
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, FIELD_CLASSROOM_SESSION, FIELD_CLASSROOM_SESSION_DIVISION
					,FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT, AbstractQueryFormModel.FIELD_IDENTIFIABLE);
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectToDynamicCreate(((ClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel)data).getIdentifiable()
						, Evaluation.class,null); 
			}
		}
	}
}