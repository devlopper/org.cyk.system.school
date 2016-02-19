package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Getter @Setter
public abstract class AbstractClassroomSessionDivisionQueryOneFormModel<SESSION extends AbstractIdentifiable> extends AbstractQueryOneFormModel.Default<SESSION> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo @NotNull protected ClassroomSession classroomSession;
	
	public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
	
	/**/
	
	@Getter @Setter
	public static class AbstractClassroomSessionDivisionSelectOnePageAdapter<SESSION extends AbstractIdentifiable> extends AbstractSelectOnePage.Listener.Adapter.Default<SESSION,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public AbstractClassroomSessionDivisionSelectOnePageAdapter(Class<SESSION> aClass) {
			super(aClass);
		}
		
	}
}