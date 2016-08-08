package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.session.AcademicSession;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class AcademicSessionConsultPage extends AbstractConsultPage<AcademicSession> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
}
