package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
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

import lombok.Getter;
import lombok.Setter;

@Getter @Setter @FieldOverrides(value={@FieldOverride(name=AbstractQueryFormModel.FIELD_IDENTIFIABLE,type=ClassroomSessionDivisionSubject.class)})
public class ClassroomSessionDivisionSubjectQueryFormModel extends AbstractQueryFormModel.Default<ClassroomSessionDivisionSubject> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo private ClassroomSessionDivision classroomSessionDivision;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo @Sequence(direction=Direction.AFTER,field=AbstractQueryFormModel.FIELD_IDENTIFIABLE)
	private ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType;
	
	@Override @Sequence(direction=Direction.AFTER,field=CLASSROOM_SESSION_DIVISION)
	public ClassroomSessionDivisionSubject getIdentifiable() {
		return super.getIdentifiable();
	}
	
	public static final String CLASSROOM_SESSION = "classroomSession";
	public static final String CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	public static final String SUBJECT_EVALUATION_TYPE = "subjectEvaluationType";
	
	/**/
	
	@Getter @Setter
	public static class PageAdapter extends SelectPageListener.Adapter.Default<ClassroomSessionDivisionSubject,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public PageAdapter() {
			super(ClassroomSessionDivisionSubject.class);
		}
		
		@Override
		public void afterInitialisationEnded(AbstractBean bean) {
			super.afterInitialisationEnded(bean);
			final SelectPage selectPage = (SelectPage) bean;
			SchoolWebManager.getInstance().initialiseSelectClassroomSession(selectPage, CLASSROOM_SESSION, CLASSROOM_SESSION_DIVISION
					, AbstractQueryFormModel.FIELD_IDENTIFIABLE,SUBJECT_EVALUATION_TYPE);
		}
		
		@Override
		public void serve(Object data, String actionIdentifier) {
			if(SchoolBusinessLayer.getInstance().getActionCreateSubjectEvaluation().equals(actionIdentifier)){
				WebNavigationManager.getInstance().redirectToDynamicCreate(((ClassroomSessionDivisionSubjectQueryFormModel)data).getSubjectEvaluationType()
						, Evaluation.class); 
			}
		}
	}
}