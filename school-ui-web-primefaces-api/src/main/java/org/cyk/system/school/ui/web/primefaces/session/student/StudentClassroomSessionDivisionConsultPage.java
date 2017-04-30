package org.cyk.system.school.ui.web.primefaces.session.student;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.session.AbstractStudentClassroomSessionDivisionSubjectDetails.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.session.StudentClassroomSessionDivision;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class StudentClassroomSessionDivisionConsultPage extends AbstractConsultPage<StudentClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	public static String SUBJECT_DETAILS_CLASS_NAME = StudentClassroomSessionDivisionSubjectDetails.class.getName();
	public static Class<AbstractStudentClassroomSessionDivisionSubjectDetails> SUBJECT_DETAILS_CLASS;
	public static Boolean LOAD_EVALUATIONS = Boolean.FALSE;
	
	private Table<AbstractStudentClassroomSessionDivisionSubjectDetails> subjectTable;
	//private List<Table<StudentResultsMetricValueDetails>> metricTables = new ArrayList<>();
	private Collection<StudentClassroomSessionDivisionSubjectEvaluation> studentSubjectEvaluations;
	private Boolean showReport = Boolean.FALSE;
		
	@SuppressWarnings("unchecked")
	@Override
	protected void consultInitialisation() {
		super.consultInitialisation();
		
		if(SUBJECT_DETAILS_CLASS==null)
			try {//TODO make it as util method
				SUBJECT_DETAILS_CLASS = (Class<AbstractStudentClassroomSessionDivisionSubjectDetails>) Class.forName(SUBJECT_DETAILS_CLASS_NAME);
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
			}
		
		if(Boolean.TRUE.equals(LOAD_EVALUATIONS)){
			studentSubjectEvaluations = inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).findByStudentByClassroomSessionDivision(
					identifiable.getStudent(),identifiable.getClassroomSessionDivision());
		}
		
		subjectTable = (Table<AbstractStudentClassroomSessionDivisionSubjectDetails>) createDetailsTable(SUBJECT_DETAILS_CLASS, 
				new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivisionSubject,AbstractStudentClassroomSessionDivisionSubjectDetails>(StudentClassroomSessionDivisionSubject.class, SUBJECT_DETAILS_CLASS){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<StudentClassroomSessionDivisionSubject> getIdentifiables() {
				Collection<StudentClassroomSessionDivisionSubject> studentSubjects = inject(StudentClassroomSessionDivisionSubjectBusiness.class)
						.findByStudentByClassroomSessionDivision(identifiable.getStudent(),identifiable.getClassroomSessionDivision());
				for(StudentClassroomSessionDivisionSubject studentSubject : studentSubjects){
					studentSubject.getDetails().clear();
					for(StudentClassroomSessionDivisionSubjectEvaluation studentSubjectEvaluation : studentSubjectEvaluations){
						if(studentSubjectEvaluation.getStudentClassroomSessionDivisionSubject().equals(studentSubject)){
							studentSubject.getDetails().add(studentSubjectEvaluation);
						}
					}
				}
				return studentSubjects;
			}
			
			@Override
			public String getTitleId() {
				return "model.entity.subject";
			}
		});
		/*for(ClassroomSessionDivisionStudentsMetricCollection classroomSessionDivisionStudentsMetricCollection : inject(ClassroomSessionDivisionStudentsMetricCollectionBusiness.class).findByClassroomSessionDivision(identifiable.getClassroomSessionDivision())){
			final MetricCollection metricCollection = classroomSessionDivisionStudentsMetricCollection.getMetricCollection();
			Table<StudentResultsMetricValueDetails> table;
			metricTables.add(table = (Table<StudentResultsMetricValueDetails>) createDetailsTable(StudentResultsMetricValueDetails.class, 
					new DetailsConfigurationListener.Table.Adapter<StudentResultsMetricValue,StudentResultsMetricValueDetails>(StudentResultsMetricValue.class, StudentResultsMetricValueDetails.class){
				private static final long serialVersionUID = 1L;
				@Override
				public Collection<StudentResultsMetricValue> getIdentifiables() {
					return inject(StudentResultsMetricValueBusiness.class).findByStudentResultsByMetricCollection(identifiable.getResults(),metricCollection);
				}
				@Override
				public String getTitleId() {
					return "model.entity.metric";
				}
				
			}));
			table.setTitle(metricCollection.getName());
			
		}*/
	}
	/*
	public Table<StudentResultsMetricValueDetails> getMetricTable(Integer index){
		//if(index < metricTables.size())
		//	return metricTables.get(index);
		return null;
	}*/
	
}
