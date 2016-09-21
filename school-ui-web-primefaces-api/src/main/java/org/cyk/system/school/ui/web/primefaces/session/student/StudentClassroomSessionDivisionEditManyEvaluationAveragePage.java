package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractEditManyPage;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditManyEvaluationAveragePage extends AbstractEditManyPage<StudentClassroomSessionDivision,StudentClassroomSessionDivisionEditPage.Many> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	public ItemCollectionWebAdapter<StudentClassroomSessionDivisionEditPage.Many, StudentClassroomSessionDivision> getItemCollectionAdapter() {
		return new StudentClassroomSessionDivisionEditPage.Many.ItemCollectionAdapter();
	}
	
}
