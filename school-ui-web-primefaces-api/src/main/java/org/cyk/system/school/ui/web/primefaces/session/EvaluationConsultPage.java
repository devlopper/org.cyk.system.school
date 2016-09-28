package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.business.impl.subject.StudentClassroomSessionDivisionSubjectEvaluationDetails;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class EvaluationConsultPage extends AbstractConsultPage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<StudentClassroomSessionDivisionSubjectEvaluationDetails> markTable;
	
	@Override
	protected void consultInitialisation() {
		super.consultInitialisation();
	
		markTable = (Table<StudentClassroomSessionDivisionSubjectEvaluationDetails>) createDetailsTable(StudentClassroomSessionDivisionSubjectEvaluationDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivisionSubjectEvaluation,StudentClassroomSessionDivisionSubjectEvaluationDetails>(StudentClassroomSessionDivisionSubjectEvaluation.class, StudentClassroomSessionDivisionSubjectEvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> getIdentifiables() {
				return inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).findByEvaluation(identifiable, Boolean.FALSE);
			}
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTabId() {
				return "model.entity.evaluation";
			}
		});
	}

}
