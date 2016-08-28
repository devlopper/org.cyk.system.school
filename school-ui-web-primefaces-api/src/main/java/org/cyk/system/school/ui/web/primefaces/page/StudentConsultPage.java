package org.cyk.system.school.ui.web.primefaces.page;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.actor.Student;
import org.cyk.ui.web.primefaces.page.party.AbstractActorConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentConsultPage extends AbstractActorConsultPage<Student> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
}
