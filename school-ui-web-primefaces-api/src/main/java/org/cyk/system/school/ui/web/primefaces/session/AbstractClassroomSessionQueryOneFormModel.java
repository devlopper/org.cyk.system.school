package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.ui.api.model.AbstractQueryOneFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectOnePage;

@Getter @Setter
public abstract class AbstractClassroomSessionQueryOneFormModel<SESSION extends AbstractIdentifiable> extends AbstractQueryOneFormModel.Default<SESSION> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	/**/
	
	@Getter @Setter
	public static class AbstractClassroomSessionSelectOnePageAdapter<SESSION extends AbstractIdentifiable> extends AbstractSelectOnePage.Listener.Adapter.Default<SESSION,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public AbstractClassroomSessionSelectOnePageAdapter(Class<SESSION> aClass) {
			super(aClass);
		}
		
	}
}