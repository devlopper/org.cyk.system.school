package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.subject.EvaluationDetails;
import org.cyk.system.school.business.impl.subject.StudentSubjectEvaluationDetails;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class SubjectEvaluationConsultPage extends AbstractConsultPage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<EvaluationDetails> details;
	private Table<StudentSubjectEvaluationDetails> markTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
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
				return SchoolBusinessLayer.getInstance().getStudentSubjectEvaluationBusiness().findBySubjectEvaluation(identifiable, Boolean.FALSE);
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

}
