package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Getter @Setter 
public abstract class AbstractClassroomSessionDivisionSubjectQueryFormModel<SESSION extends AbstractIdentifiable> extends AbstractClassroomSessionDivisionQueryOneFormModel<SESSION> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo @NotNull private ClassroomSessionDivision classroomSessionDivision;
	
	public static final String FIELD_CLASSROOM_SESSION_DIVISION = "classroomSessionDivision";
	
	/**/
	
	@Getter @Setter
	public static abstract class AbstractClassroomSessionDivisionSubjectPageAdapter<SESSION extends AbstractIdentifiable> extends AbstractSelectOnePage.Listener.Adapter.Default<SESSION,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public AbstractClassroomSessionDivisionSubjectPageAdapter(Class<SESSION> aClass) {
			super(aClass);
		}
		
	}
}