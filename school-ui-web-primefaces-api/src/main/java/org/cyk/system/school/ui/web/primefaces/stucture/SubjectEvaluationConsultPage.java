package org.cyk.system.school.ui.web.primefaces.stucture;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionDivisionBusiness;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
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
public class SubjectEvaluationConsultPage extends AbstractConsultPage<SubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Inject private ClassroomSessionBusiness classroomSessionBusiness;
	@Inject private ClassroomSessionDivisionBusiness classroomSessionDivisionBusiness;
	
	private FormOneData<Details> subjectEvaluationDetails;
	private Table<MarkDetails> markTable;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		/*contentTitle = classroomSessionBusiness.format(identifiable.getClassroomSessionDivision().getClassroomSession())
				+" : "+classroomSessionDivisionBusiness.format(identifiable.getClassroomSessionDivision())
				+" : "+identifiable.getSubject().getName();*/
		
		subjectEvaluationDetails = (FormOneData<Details>) createFormOneData(new Details(identifiable), Crud.READ);
		configureDetailsForm(subjectEvaluationDetails);
		
		markTable = (Table<MarkDetails>) createTable(MarkDetails.class, null, null);
		configureDetailsTable(markTable, "model.entity.studentSubjectEvaluation");
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		for(StudentSubjectEvaluation studentSubjectEvaluation : identifiable.getStudentSubjectEvaluations()){
			markTable.addRow(new MarkDetails(studentSubjectEvaluation));	
		}
		//classroomSessionDivisionSubjectTable.setShowEditColumn(Boolean.TRUE);
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		commandable = navigationManager.createConsultCommandable(identifiable.getType().getSubject(), "button", null);
		commandable.setLabel(identifiable.getType().getSubject().getSubject().getName());
		contextualMenu.getChildren().add(commandable);
		
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
			type = subjectEvaluation.getType().getName().getName();
			coefficient = numberBusiness.format(subjectEvaluation.getType().getCoefficient());
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
