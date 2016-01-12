package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.business.impl.session.ClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.session.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.business.impl.subject.EvaluationDetails;
import org.cyk.system.school.business.impl.subject.LectureDetails;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.data.collector.form.FormOneData;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionSubjectConsultPage extends AbstractConsultPage<ClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private FormOneData<ClassroomSessionDivisionSubjectDetails> details;
	private Table<StudentClassroomSessionDivisionSubjectDetails> studentTable;
	private Table<EvaluationDetails> evaluationTable;
	private Table<LectureDetails> lectureTable;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		contentTitle = formatPathUsingBusiness(ClassroomSession.class,identifiable);
		
		details = createDetailsForm(ClassroomSessionDivisionSubjectDetails.class, identifiable, new DetailsConfigurationListener.Form.Adapter<ClassroomSessionDivisionSubject,ClassroomSessionDivisionSubjectDetails>(ClassroomSessionDivisionSubject.class, ClassroomSessionDivisionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Boolean getEnabledInDefaultTab() {
				return Boolean.TRUE;
			}
		});
		
		studentTable = (Table<StudentClassroomSessionDivisionSubjectDetails>) createDetailsTable(StudentClassroomSessionDivisionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentSubject,StudentClassroomSessionDivisionSubjectDetails>(StudentSubject.class, StudentClassroomSessionDivisionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override 
			public Collection<StudentSubject> getIdentifiables() {
				return SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().findByClassroomSessionDivisionSubject(identifiable);
			}
			@Override
			public Crud[] getCruds() {
				return new Crud[]{/*Crud.CREATE*/};
			}
		});
		
		evaluationTable = (Table<EvaluationDetails>) createDetailsTable(EvaluationDetails.class, new DetailsConfigurationListener.Table.Adapter<Evaluation,EvaluationDetails>(Evaluation.class, EvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<Evaluation> getIdentifiables() {
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
}
