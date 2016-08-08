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
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class EvaluationConsultPage extends AbstractConsultPage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<StudentSubjectEvaluationDetails> markTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
	
		markTable = (Table<StudentSubjectEvaluationDetails>) createDetailsTable(StudentSubjectEvaluationDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivisionSubjectEvaluation,StudentSubjectEvaluationDetails>(StudentClassroomSessionDivisionSubjectEvaluation.class, StudentSubjectEvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getStudentClassroomSessionDivisionSubjectEvaluationBusiness().findByEvaluation(identifiable, Boolean.FALSE);
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
