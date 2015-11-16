package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.StudentResultsMetricValueBusiness;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.SchoolReportRepository;
import org.cyk.system.school.model.StudentResultsMetricValue;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.StudentResultsMetricValueDetails;
import org.cyk.ui.api.UIProvider;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.command.UICommandable.CommandRequestType;
import org.cyk.ui.api.command.UICommandable.ViewType;
import org.cyk.ui.api.model.AbstractOutputDetails;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionConsultPage extends AbstractConsultPage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	public static String SUBJECT_DETAILS_CLASS_NAME = SubjectDetails.class.getName();
	public static Class<AbstractSubjectDetails> SUBJECT_DETAILS_CLASS;
	public static Boolean LOAD_EVALUATIONS = Boolean.FALSE;
	
	private FormOneData<Details> details;
	private Table<AbstractSubjectDetails> subjectTable;
	private Table<StudentResultsMetricValueDetails> metricTable;
	private Collection<StudentSubjectEvaluation> studentSubjectEvaluations;
	private String tab1TitleId="tab1",tab2TitleId="tab2";
	private Boolean showReport = Boolean.FALSE;
	
	@Inject private StudentResultsMetricValueBusiness studentResultsMetricValueBusiness;
	
	@SuppressWarnings("unchecked")
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(identifiable.getClassroomSessionDivision())+" : "
				+identifiable.getStudent().getPerson().getNames();
		
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
		
		details = createDetailsForm(Details.class, identifiable, new DetailsFormOneDataConfigurationAdapter<ClassroomSessionDivision,Details>(ClassroomSessionDivision.class, Details.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTitleId() {
				return tab1TitleId;
			}
		});
		
		subjectTable = (Table<AbstractSubjectDetails>) createDetailsTable(SUBJECT_DETAILS_CLASS, 
				new DetailsTableConfigurationAdapter<StudentSubject,AbstractSubjectDetails>(StudentSubject.class, SUBJECT_DETAILS_CLASS){
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
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTabId() {
				return tab1TitleId;
			}
		});
		
		metricTable = (Table<StudentResultsMetricValueDetails>) createDetailsTable(StudentResultsMetricValueDetails.class, 
				new DetailsTableConfigurationAdapter<StudentResultsMetricValue,StudentResultsMetricValueDetails>(StudentResultsMetricValue.class, StudentResultsMetricValueDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentResultsMetricValue> getIdentifiables() {
				return studentResultsMetricValueBusiness.findByStudentResults(identifiable.getResults());
			}
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
			@Override
			public String getTabId() {
				return tab1TitleId;
			}
		});
		
		
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null),commandable=null;
		contextualMenu.setLabel(contentTitle); 
		
		UICommandable printReceipt = UIProvider.getInstance().createCommandable("command.see.report", null);
		printReceipt.setCommandRequestType(CommandRequestType.UI_VIEW);
		printReceipt.setViewType(ViewType.TOOLS_REPORT);
		printReceipt.getParameters().addAll(navigationManager.reportParameters(identifiable, SchoolReportRepository.getInstance().getReportStudentClassroomSessionDivision(),Boolean.FALSE));
		contextualMenu.getChildren().add(printReceipt);
		
		return Arrays.asList(contextualMenu);
	}
	
	/**/
	
	public static class Details extends AbstractOutputDetails<StudentClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText private String registrationCode,names,classroomSession,classroomSessionDivision,numberOfTimeSchoolOpened,numberOfTimePresent,numberOfTimeAbsent;
		public Details(StudentClassroomSessionDivision studentClassroomSessionDivision) {
			super(studentClassroomSessionDivision);
			registrationCode = studentClassroomSessionDivision.getStudent().getRegistration().getCode();
			names = studentClassroomSessionDivision.getStudent().getPerson().getNames();
			classroomSession = SchoolBusinessLayer.getInstance().getClassroomSessionBusiness().format(studentClassroomSessionDivision.getClassroomSessionDivision().getClassroomSession());
			classroomSessionDivision = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionBusiness().format(studentClassroomSessionDivision.getClassroomSessionDivision());
		}
	}
	
	public static abstract class AbstractSubjectDetails extends AbstractOutputDetails<StudentSubject> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputText protected String subject,total/*,coefficient/*,grade,rank,outof,max,classAverage,remarks*/,teacher;
		public AbstractSubjectDetails(StudentSubject studentSubject) {
			super(studentSubject);
			subject = studentSubject.getClassroomSessionDivisionSubject().getSubject().getName();
			//coefficient = numberBusiness.format(studentSubject.getClassroomSessionDivisionSubject().getCoefficient());
			teacher = studentSubject.getClassroomSessionDivisionSubject().getTeacher().getPerson().getNames();
			
		}
		
		public static final String FILED_SUBJECT = "subject";
	}
	
	public static class SubjectDetails extends AbstractSubjectDetails implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		public SubjectDetails(StudentSubject studentSubject) {
			super(studentSubject);
		}
	}
	
}
