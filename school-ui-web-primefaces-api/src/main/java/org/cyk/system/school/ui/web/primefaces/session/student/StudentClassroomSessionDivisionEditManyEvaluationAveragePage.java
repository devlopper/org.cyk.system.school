package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.util.List;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.mathematics.IntervalBusiness;
import org.cyk.system.root.model.mathematics.Interval;
import org.cyk.system.school.business.api.session.AcademicSessionBusiness;
import org.cyk.system.school.model.session.StudentClassroomSession;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractEditManyPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionEditManyEvaluationAveragePage extends AbstractEditManyPage<StudentClassroomSessionDivision,StudentClassroomSessionDivisionEditPage.Many> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	private List<SelectItem> gradeIntervals;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		gradeIntervals = webManager.getSelectItems(Interval.class, inject(IntervalBusiness.class)
				.findByCollection(inject(AcademicSessionBusiness.class).findCurrent(null).getNodeInformations().getStudentClassroomSessionAverageScale()));
		for(SelectItem selectItem : gradeIntervals)
			if(selectItem.getValue()!=null)
				selectItem.setLabel( ((Interval)selectItem.getValue()).getName());
			
	}
	
	@Override
	public ItemCollectionWebAdapter<StudentClassroomSessionDivisionEditPage.Many, StudentClassroomSessionDivision,StudentClassroomSession> getItemCollectionAdapter() {
		return new StudentClassroomSessionDivisionEditPage.Many.ItemCollectionAdapter();
	}
	
}
