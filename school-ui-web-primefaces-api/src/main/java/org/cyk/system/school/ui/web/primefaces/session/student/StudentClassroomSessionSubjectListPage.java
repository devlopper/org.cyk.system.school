package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudManyPage;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionSubjectListPage extends AbstractCrudManyPage<StudentClassroomSessionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
}
