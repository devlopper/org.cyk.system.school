package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionSubjectConsultPage extends AbstractConsultPage<ClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<Details> details;
	private Table<EvaluationDetails> evaluationTable;
	private Table<LectureDetails> lectureTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = identifiable.getSubject().getName();
		
		details = createDetailsForm(Details.class, identifiable, new DetailsFormOneDataConfigurationAdapter<ClassroomSessionDivisionSubject,Details>(ClassroomSessionDivisionSubject.class, Details.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		evaluationTable = (Table<EvaluationDetails>) createDetailsTable(EvaluationDetails.class, new DetailsTableConfigurationAdapter<SubjectEvaluation,EvaluationDetails>(SubjectEvaluation.class, EvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<SubjectEvaluation> getIdentifiables() {
				return identifiable.getEvaluations();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
		
		lectureTable = (Table<LectureDetails>) createDetailsTable(LectureDetails.class, new DetailsTableConfigurationAdapter<Lecture,LectureDetails>(Lecture.class, LectureDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<Lecture> getIdentifiables() {
				return identifiable.getLectures();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.READ,Crud.UPDATE};
			}
		});
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		commandable = navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivision(), "button", null);
		commandable.setLabel(SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(identifiable.getClassroomSessionDivision()));
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
			name = subjectEvaluation.getType().getType().getName();
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
