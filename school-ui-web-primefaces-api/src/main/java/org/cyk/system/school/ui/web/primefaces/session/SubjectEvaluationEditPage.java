package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.api.model.ItemCollectionListener.ItemCollectionAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputCalendar;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class SubjectEvaluationEditPage extends AbstractCrudOnePage<SubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ItemCollection<Mark,StudentSubjectEvaluation> markCollection;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		ClassroomSessionDivisionSubject classroomSessionDivisionSubject = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness()
				.find(requestParameterLong(uiManager.businessEntityInfos(ClassroomSessionDivisionSubject.class).getIdentifier()));
		Collection<StudentSubjectEvaluation> studentSubjectEvaluations=null;
		if(Crud.CREATE.equals(crud)){
			studentSubjectEvaluations = new ArrayList<>();
			for(StudentSubject studentSubject : SchoolBusinessLayer.getInstance().getStudentSubjectBusiness().findBySubject(classroomSessionDivisionSubject)){
				studentSubjectEvaluations.add(new StudentSubjectEvaluation(identifiable, studentSubject, null));
			}
		}else
			;//studentSubjectEvaluations = SchoolBusinessLayer.getInstance().getStudentSubjectEvaluationBusiness().findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
		
		markCollection = createItemCollection(form, "qwerty", Mark.class, StudentSubjectEvaluation.class, studentSubjectEvaluations,new ItemCollectionAdapter<Mark,StudentSubjectEvaluation>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<Mark, StudentSubjectEvaluation> itemCollection,Mark mark) {
				super.instanciated(itemCollection, mark);
				mark.setRegistrationCode(mark.getIdentifiable().getStudentSubject().getStudent().getRegistration().getCode());
				mark.setNames(mark.getIdentifiable().getStudentSubject().getStudent().getPerson().getNames());
			}	
		});
	}
	
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Getter @Setter
	public static class Form extends AbstractFormModel<SubjectEvaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		//@Input @InputOneChoice @InputOneCombo private AcademicSession academicSession;
		//@Input @InputOneChoice @InputOneCombo private Teacher coordinator;
		
		@Input @InputOneChoice @InputOneCombo @NotNull private SubjectEvaluationType type;
		@Input @InputCalendar @NotNull private Date date;
		@NotNull private Boolean coefficientApplied = Boolean.TRUE;
		
		
	}
	
	@Getter @Setter
	public static class Mark extends AbstractItemCollectionItem<StudentSubjectEvaluation> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private String registrationCode;
		private String names;
		@Input @InputNumber @NotNull private BigDecimal value;
	}

}
