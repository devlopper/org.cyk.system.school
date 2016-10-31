package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.util.Collection;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.impl.subject.ClassroomSessionDivisionSubjectEvaluationTypeDetails;
import org.cyk.system.school.business.impl.subject.EvaluationDetails;
import org.cyk.system.school.business.impl.subject.LectureDetails;
import org.cyk.system.school.business.impl.subject.StudentClassroomSessionDivisionSubjectDetails;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.ui.web.primefaces.Table;
import org.cyk.ui.web.primefaces.page.crud.AbstractConsultPage;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionSubjectConsultPage extends AbstractConsultPage<ClassroomSessionDivisionSubject> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private Table<ClassroomSessionDivisionSubjectEvaluationTypeDetails> evaluationTypeTable;
	
	private Table<StudentClassroomSessionDivisionSubjectDetails> studentTable;
	private Table<EvaluationDetails> evaluationTable;
	private Table<LectureDetails> lectureTable;
	
	@Override
	protected void consultInitialisation() {
		super.consultInitialisation();
		
		evaluationTypeTable = (Table<ClassroomSessionDivisionSubjectEvaluationTypeDetails>) createDetailsTable(ClassroomSessionDivisionSubjectEvaluationTypeDetails.class, new DetailsConfigurationListener.Table.Adapter<ClassroomSessionDivisionSubjectEvaluationType,ClassroomSessionDivisionSubjectEvaluationTypeDetails>(ClassroomSessionDivisionSubjectEvaluationType.class, ClassroomSessionDivisionSubjectEvaluationTypeDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<ClassroomSessionDivisionSubjectEvaluationType> getIdentifiables() {
				return inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(identifiable);
			}
		});
		
		studentTable = (Table<StudentClassroomSessionDivisionSubjectDetails>) createDetailsTable(StudentClassroomSessionDivisionSubjectDetails.class, new DetailsConfigurationListener.Table.Adapter<StudentClassroomSessionDivisionSubject,StudentClassroomSessionDivisionSubjectDetails>(StudentClassroomSessionDivisionSubject.class, StudentClassroomSessionDivisionSubjectDetails.class){
			private static final long serialVersionUID = 1L;
			@Override 
			public Collection<StudentClassroomSessionDivisionSubject> getIdentifiables() {
				return inject(StudentClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionSubject(identifiable);
			}
		});
		
		evaluationTable = (Table<EvaluationDetails>) createDetailsTable(EvaluationDetails.class, new DetailsConfigurationListener.Table.Adapter<Evaluation,EvaluationDetails>(Evaluation.class, EvaluationDetails.class){
			private static final long serialVersionUID = 1L;
			@Override
			public Collection<Evaluation> getIdentifiables() {
				return inject(EvaluationBusiness.class).findByClassroomSessionDivisionSubject(identifiable);
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
