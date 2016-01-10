package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.impl.subject.EvaluationDetails;
import org.cyk.system.school.business.impl.subject.StudentSubjectEvaluationDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class SubjectEvaluationConsultPage extends AbstractConsultPage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<EvaluationDetails> details;
	private Table<StudentSubjectEvaluationDetails> markTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = formatPathUsingBusiness(ClassroomSession.class,identifiable.getClassroomSessionDivisionSubjectEvaluationType());
		
		details = createDetailsForm(EvaluationDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<Evaluation,EvaluationDetails>(Evaluation.class, EvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTabId() {
				return "1";
			}
		});
		
		markTable = (Table<StudentSubjectEvaluationDetails>) createDetailsTable(StudentSubjectEvaluationDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentSubjectEvaluation,StudentSubjectEvaluationDetails>(StudentSubjectEvaluation.class, StudentSubjectEvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentSubjectEvaluation> getIdentifiables() {
				return identifiable.getStudentSubjectEvaluations();
			}
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTabId() {
				return "1";
			}
		});
	}

	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		contextualMenu.getChildren().add(navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession(), null));
		contextualMenu.getChildren().add(navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision(), null));
		contextualMenu.getChildren().add(navigationManager.createConsultCommandable(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject(), null));
		
		commandable = navigationManager.createUpdateCommandable(identifiable, "command.edit", null);
		contextualMenu.getChildren().add(commandable);
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	/*public static class Details extends AbstractOutputDetails<Evaluationd> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String date,type,coefficient;
		public Details(Evaluation subjectEvaluation) {
			super(subjectEvaluation);
			date = timeBusiness.formatDate(subjectEvaluation.getDate());
			type = subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getName();
			coefficient = numberBusiness.format(subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getCoefficient());
		}
	}*/
	
	

}
