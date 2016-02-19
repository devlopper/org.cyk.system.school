package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.ui.api.model.AbstractQueryManyFormModel;
import org.cyk.ui.web.primefaces.page.AbstractSelectManyPage;

@Getter @Setter
public abstract class AbstractClassroomSessionQueryManyFormModel<SESSION extends AbstractIdentifiable> extends AbstractQueryManyFormModel.Default<SESSION> implements Serializable {
	private static final long serialVersionUID = -3756660150800681378L;
	
	/**/
	
	@Getter @Setter
	public static class AbstractClassroomSessionSelectManyPageAdapter<SESSION extends AbstractIdentifiable> extends AbstractSelectManyPage.Listener.Adapter.Default<SESSION,String> implements Serializable {

		private static final long serialVersionUID = -7392513843271510254L;
		
		public AbstractClassroomSessionSelectManyPageAdapter(Class<SESSION> aClass) {
			super(aClass);
		}
		
	}
}