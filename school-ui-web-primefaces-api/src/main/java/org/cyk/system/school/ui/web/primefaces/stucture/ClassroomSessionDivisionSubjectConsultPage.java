package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.api.model.table.Row;
import org.cyk.ui.api.model.table.RowAdapter;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionSubjectConsultPage extends AbstractConsultPage<ClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	
	private FormOneData<Details> classroomSessionDivisionSubjectDetails;
	private Table<EvaluationDetails> evaluationTable;
	private Table<LectureDetails> lectureTable;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = classroomSessionBusiness.format(identifiable.getClassroomSessionDivision().getClassroomSession())
				+" : "+classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision())
				+" : "+identifiable.getSubject().getName();
		
		classroomSessionDivisionSubjectDetails = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(classroomSessionDivisionSubjectDetails);
		
		evaluationTable = (Table<EvaluationDetails>) createTable(EvaluationDetails.class, null, null);
		configureDetailsTable(evaluationTable, "model.entity.subjectEvaluation",Boolean.TRUE);
		
		evaluationTable.getRowListeners().add(new RowAdapter<EvaluationDetails>(){
			@Override
			public void added(Row<EvaluationDetails> row) {
				super.added(row);
				row.setOpenable(Boolean.TRUE);
				row.setUpdatable(Boolean.TRUE);
			}
		});
		
		lectureTable = (Table<LectureDetails>) createTable(LectureDetails.class, null, null);
		configureDetailsTable(lectureTable, "model.entity.lecture",Boolean.TRUE);
		
		lectureTable.getRowListeners().add(new RowAdapter<LectureDetails>(){
			@Override
			public void added(Row<LectureDetails> row) {
				super.added(row);
				row.setOpenable(Boolean.TRUE);
				row.setUpdatable(Boolean.TRUE);
			}
		});
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		for(SubjectEvaluation evaluation : identifiable.getEvaluations()){
			evaluationTable.addRow(new EvaluationDetails(evaluation));	
		}
		for(Lecture lecture  : identifiable.getLectures()){
			lectureTable.addRow(new LectureDetails(lecture));	
		}
		//classroomSessionDivisionSubjectTable.setShowEditColumn(Boolean.TRUE);
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		commandable = navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivision(), "button", null);
		commandable.setLabel(classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision()));
		contextualMenu.getChildren().add(commandable);
		
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		
		commandable = navigationManager.createCreateCommandable(Lecture.class, uiManager.businessEntityInfos(Lecture.class).getUiLabelId(), null);
		contextualMenu.getChildren().add(commandable);
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<ClassroomSessionDivisionSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,coefficient,teacher;
		
		public Details(ClassroomSessionDivisionSubject classroomSessionDivisionSubject) {
			super(classroomSessionDivisionSubject);
			name = classroomSessionDivisionSubject.getSubject().getName();
			coefficient = numberBusiness.format(classroomSessionDivisionSubject.getCoefficient());
			teacher = classroomSessionDivisionSubject.getTeacher().getPerson().getNames();
		}
		
	}
	
	public static class EvaluationDetails extends AbstractOutputDetails<SubjectEvaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String name,coefficient;
		
		public EvaluationDetails(SubjectEvaluation subjectEvaluation) {
			super(subjectEvaluation);
			name = subjectEvaluation.getType().getName().getName();
			coefficient = numberBusiness.format(subjectEvaluation.getType().getCoefficient());
		}
		
	}
	
	public static class LectureDetails extends AbstractOutputDetails<Lecture> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputText private String date;
		
		public LectureDetails(Lecture lecture) {
			super(lecture);
			date = timeBusiness.formatDate(lecture.getEvent().getPeriod().getFromDate());
		}
		
	}

}
