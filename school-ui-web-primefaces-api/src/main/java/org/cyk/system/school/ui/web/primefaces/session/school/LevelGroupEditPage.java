package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.party.person.Person;
import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LevelGroupEditPage extends AbstractCrudOnePage<LevelGroup> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
			
	public static class Form extends AbstractFormModel<LevelGroup> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		private Person person;
			
	}

}
