package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.system.root.business.impl.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class SubjectEvaluationConsultPage extends AbstractConsultPage<SubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<Details> details;
	private Table<MarkDetails> markTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = formatPathUsingBusiness(ClassroomSession.class,identifiable.getClassroomSessionDivisionSubjectEvaluationType());
		
		details = createDetailsForm(Details.class, identifiable, new DetailsConfigurationListener.Form.Adapter<SubjectEvaluation,Details>(SubjectEvaluation.class, Details.class){
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
		
		markTable = (Table<MarkDetails>) createDetailsTable(MarkDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentSubjectEvaluation,MarkDetails>(StudentSubjectEvaluation.class, MarkDetails.class){
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
	
	public static class Details extends AbstractOutputDetails<SubjectEvaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String date,type,coefficient;
		public Details(SubjectEvaluation subjectEvaluation) {
			super(subjectEvaluation);
			date = timeBusiness.formatDate(subjectEvaluation.getDate());
			type = subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getEvaluationType().getName();
			coefficient = numberBusiness.format(subjectEvaluation.getClassroomSessionDivisionSubjectEvaluationType().getCoefficient());
		}
	}
	
	public static class MarkDetails extends AbstractOutputDetails<StudentSubjectEvaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String student,mark;
		public MarkDetails(StudentSubjectEvaluation studentSubjectEvaluation) {
			super(studentSubjectEvaluation);
			student = studentSubjectEvaluation.getStudentSubject().getStudent().getPerson().getNames();
			mark = numberBusiness.format(studentSubjectEvaluation.getValue());
		}
		
	}

}
