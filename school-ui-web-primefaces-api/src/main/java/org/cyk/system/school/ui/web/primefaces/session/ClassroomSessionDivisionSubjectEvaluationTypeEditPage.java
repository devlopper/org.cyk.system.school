package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionSubjectEvaluationTypeEditPage extends AbstractCrudOnePage<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractFormModel<ClassroomSessionDivisionSubjectEvaluationType> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		
	}

}
