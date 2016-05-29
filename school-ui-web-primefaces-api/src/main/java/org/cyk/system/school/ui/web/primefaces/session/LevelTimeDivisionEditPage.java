package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LevelTimeDivisionEditPage extends AbstractCrudOnePage<LevelTimeDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractFormModel<LevelTimeDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		
		
	}

}
