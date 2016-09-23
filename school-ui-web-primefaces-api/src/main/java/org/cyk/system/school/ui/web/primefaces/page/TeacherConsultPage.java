package org.cyk.system.school.ui.web.primefaces.page;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Teacher;
import org.cyk.ui.web.primefaces.page.party.AbstractActorConsultPage;

@Named @ViewScoped @Getter @Setter
public class TeacherConsultPage extends AbstractActorConsultPage<Teacher> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
}
