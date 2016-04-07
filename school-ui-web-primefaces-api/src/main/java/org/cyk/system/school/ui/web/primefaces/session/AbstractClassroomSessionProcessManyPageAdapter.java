package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.AbstractIdentifiable;
import org.cyk.ui.web.primefaces.page.AbstractProcessManyPage;

@Getter @Setter
public abstract class AbstractClassroomSessionProcessManyPageAdapter<SESSION extends AbstractIdentifiable> extends AbstractProcessManyPage.Listener.Adapter.Default<SESSION,String> implements Serializable {

	private static final long serialVersionUID = -7392513843271510254L;
	
	public AbstractClassroomSessionProcessManyPageAdapter(Class<SESSION> aClass) {
		super(aClass);
	}
		
}