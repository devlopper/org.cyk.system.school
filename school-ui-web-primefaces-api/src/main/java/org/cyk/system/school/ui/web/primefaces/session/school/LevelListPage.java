package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.Level;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudManyPage;

@Named @ViewScoped @Getter @Setter
public class LevelListPage extends AbstractCrudManyPage<Level> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
}
