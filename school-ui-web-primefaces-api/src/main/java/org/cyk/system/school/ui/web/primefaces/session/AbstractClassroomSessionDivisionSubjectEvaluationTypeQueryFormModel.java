package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Getter @Setter 
public abstract class AbstractClassroomSessionDivisionSubjectEvaluationTypeQueryFormModel<SESSION extends AbstractIdentifiable> extends AbstractClassroomSessionDivisionSubjectQueryFormModel<SESSION> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo /*@Sequence(direction=Direction.AFTER,field=AbstractQueryFormModel.FIELD_IDENTIFIABLE)*/ @NotNull
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	
	public static final String FIELD_CLASSROOM_SESSION_DIVISION_SUBJECT = "classroomSessionDivisionSubject";
	
	/**/
	
	@Getter @Setter
	public static abstract class AbstractClassroomSessionDivisionSubjectEvaluationTypePageAdapter<SESSION extends AbstractIdentifiable> extends AbstractSelectOnePage.Listener.Adapter.Default<SESSION,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public AbstractClassroomSessionDivisionSubjectEvaluationTypePageAdapter(Class<SESSION> aClass) {
			super(aClass);
		}
		
	}
}