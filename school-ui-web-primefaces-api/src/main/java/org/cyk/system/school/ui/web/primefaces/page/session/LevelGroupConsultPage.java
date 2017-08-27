package org.cyk.system.school.ui.web.primefaces.page.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.LevelGroup;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class LevelGroupConsultPage extends AbstractConsultPage<LevelGroup> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
}
