package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.School;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class SchoolConsultPage extends AbstractConsultPage<School> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
}
