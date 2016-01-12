package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolReportRepository;
import org.cyk.system.school.business.impl.session.AbstractSubjectDetails;
import org.cyk.system.school.business.impl.session.AbstractSubjectDetails.SubjectDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionDetails;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.StudentResultsMetricValueDetails;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionConsultPage extends AbstractConsultPage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	public static String SUBJECT_DETAILS_CLASS_NAME = SubjectDetails.class.getName();
	public static Class<AbstractSubjectDetails> SUBJECT_DETAILS_CLASS;
	public static Boolean LOAD_EVALUATIONS = Boolean.FALSE;
	
	private FormOneData<StudentClassroomSessionDivisionDetails> details;
	private Table<AbstractSubjectDetails> subjectTable;
	private Table<StudentResultsMetricValueDetails> metricTable;
	private Collection<StudentSubjectEvaluation> studentSubjectEvaluations;
	private Boolean showReport = Boolean.FALSE;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		
		if(SUBJECT_DETAILS_CLASS==null)
			try {//TODO make it as util method
				SUBJECT_DETAILS_CLASS = (Class<AbstractSubjectDetails>) Class.forName(SUBJECT_DETAILS_CLASS_NAME);
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
			}
		
		if(Boolean.TRUE.equals(LOAD_EVALUATIONS)){
			studentSubjectEvaluations = SchoolBusinessLayer.getInstance().getStudentSubjectEvaluationBusiness().findByStudentByClassroomSessionDivision(
					identifiable.getStudent(),identifiable.getClassroomSessionDivision());
		}
		
		details = createDetailsForm(StudentClassroomSessionDivisionDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<StudentClassroomSessionDivision,StudentClassroomSessionDivisionDetails>(StudentClassroomSessionDivision.class, StudentClassroomSessionDivisionDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTitleId() {
				return "model.entity.student";
			}
		});
		
		subjectTable = (Table<AbstractSubjectDetails>) createDetailsTable(SUBJECT_DETAILS_CLASS, 
				new DetailsConfigurationListener.Table.Adapter<StudentSubject,AbstractSubjectDetails>(StudentSubject.class, SUBJECT_DETAILS_CLASS){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentSubject> getIdentifiables() {
				Collection<StudentSubject> studentSubjects = SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().findByStudentByClassroomSessionDivision(identifiable.getStudent(),identifiable.getClassroomSessionDivision());
				for(StudentSubject studentSubject : studentSubjects){
					studentSubject.getDetails().clear();
					for(StudentSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluations)
						if(studentSubjectEvaluation.getStudentSubject().getIdentifier().equals(studentSubject.getIdentifier())){
							studentSubject.getDetails().add(studentSubjectEvaluation);
						}
				}
				return studentSubjects;
			}
			@Override
			public String getTitleId() {
				return "model.entity.subject";
			}
		});
		
		metricTable = (Table<StudentResultsMetricValueDetails>) createDetailsTable(StudentResultsMetricValueDetails.class, 
				new DetailsConfigurationListener.Table.Adapter<StudentResultsMetricValue,StudentResultsMetricValueDetails>(StudentResultsMetricValue.class, StudentResultsMetricValueDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentResultsMetricValue> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getStudentResultsMetricValueBusiness().findByStudentResults(identifiable.getResults());
			}
			@Override
			public String getTitleId() {
				return "model.entity.metric";
			}
		});
		
		
	}
	
	@Override
	protected void processIdentifiableContextualCommandable(UICommandable commandable) {
		super.processIdentifiableContextualCommandable(commandable);
		if(identifiable.getResults().getReport()!=null)
			commandable.addChild(navigationManager.createReportCommandable(identifiable, SchoolReportRepository.getInstance().getReportStudentClassroomSessionDivision()
					,"school.markscard", null));
	}
	
}
