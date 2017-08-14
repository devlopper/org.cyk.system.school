package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.subject.StudentClassroomSessionSubject;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionSubjectEditPage extends AbstractCrudOnePage<StudentClassroomSessionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	public static class Form extends AbstractFormModel<StudentClassroomSessionSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
	}
	
}
