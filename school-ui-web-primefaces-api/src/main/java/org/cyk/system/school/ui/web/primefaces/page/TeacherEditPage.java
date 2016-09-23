package org.cyk.system.school.ui.web.primefaces.page;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.ui.api.model.party.AbstractActorEditFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

@Named @ViewScoped @Getter @Setter
public class TeacherEditPage extends AbstractCrudOnePage<Teacher> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractActorEditFormModel.AbstractDefault.Default<Teacher> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
	}

}
