package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudManyPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class LevelTimeDivisionListPage extends AbstractCrudManyPage<LevelTimeDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
}
