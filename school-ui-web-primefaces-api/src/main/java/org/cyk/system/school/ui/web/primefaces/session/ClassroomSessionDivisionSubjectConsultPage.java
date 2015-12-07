package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Lecture;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
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
	
	private FormOneData<ClassroomSessionDivisionSubjectDetails> details;
	private Table<EvaluationDetails> evaluationTable;
	private Table<LectureDetails> lectureTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = identifiable.getSubject().getName();
		
		details = createDetailsForm(ClassroomSessionDivisionSubjectDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<ClassroomSessionDivisionSubject,ClassroomSessionDivisionSubjectDetails>(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		evaluationTable = (Table<EvaluationDetails>) createDetailsTable(EvaluationDetails.class, new DetailsConfigurationListener.Table.Adapter<SubjectEvaluation,EvaluationDetails>(SubjectEvaluation.class, EvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<SubjectEvaluation> getIdentifiables() {
				return identifiable.getEvaluations();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.CREATE,Crud.READ,Crud.UPDATE,Crud.DELETE};
			}
		});
		/*
		lectureTable = (Table<LectureDetails>) createDetailsTable(LectureDetails.class, new DetailsConfigurationListener.Table.Adapter<Lecture,LectureDetails>(Lecture.class, LectureDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<Lecture> getIdentifiables() {
				return identifiable.getLectures();
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{Crud.CREATE,Crud.READ,Crud.UPDATE,Crud.DELETE};
			}
		});*/
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		contextualMenu.getChildren().add(commandable = navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivision().getClassroomSession(), null));
		
		contextualMenu.getChildren().add(commandable = navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivision(), null));
		
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		
		commandable = navigationManager.createCreateCommandable(Lecture.class, uiManager.businessEntityInfos(Lecture.class).getUiLabelId(), null);
		contextualMenu.getChildren().add(commandable);
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
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
