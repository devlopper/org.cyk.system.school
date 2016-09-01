package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.command.AbstractCommandable.Builder;
import org.cyk.ui.api.command.UICommandable;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.AbstractWebApplicableValueQuestion;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class EvaluationEditPage extends AbstractCrudOnePage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	private ClassroomSessionDivisionSubjectEvaluationType subjectEvaluationType;
	private ItemCollection<StudentSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation> markCollection;
	private BigDecimal maximumValue;
	private Integer decimalPlaces = 0;
	
	@Inject private SchoolWebManager schoolWebManager;
	
	@Override
	protected void initialisation() {
		Long subjectEvaluationTypeIdentifier = requestParameterLong(ClassroomSessionDivisionSubjectEvaluationType.class);
		if(subjectEvaluationTypeIdentifier==null){
			Long classroomSessionDivisionSubjectIdentifier = requestParameterLong(ClassroomSessionDivisionSubject.class);
			if(classroomSessionDivisionSubjectIdentifier==null)
				;
			else
				classroomSessionDivisionSubject = inject(ClassroomSessionDivisionSubjectBusiness.class).find(classroomSessionDivisionSubjectIdentifier);	
		}else{
			subjectEvaluationType = inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).find(subjectEvaluationTypeIdentifier);
			classroomSessionDivisionSubject = subjectEvaluationType.getClassroomSessionDivisionSubject();
		}
			
		super.initialisation();
		if(subjectEvaluationType!=null){
			maximumValue = identifiable.getClassroomSessionDivisionSubjectEvaluationType().getMaximumValue();
		}
		if(Crud.CREATE.equals(crud)){
			
		}else{
			identifiable.setStudentSubjectEvaluations(inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).findByEvaluation(identifiable,Crud.UPDATE.equals(crud)));
			subjectEvaluationType = identifiable.getClassroomSessionDivisionSubjectEvaluationType();
			classroomSessionDivisionSubject = subjectEvaluationType.getClassroomSessionDivisionSubject();
		}
		
		markCollection = createItemCollection(StudentSubjectEvaluationItem.class, StudentClassroomSessionDivisionSubjectEvaluation.class,new ItemCollectionWebAdapter<StudentSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> create() {
				return identifiable.getStudentSubjectEvaluations();
			}
			@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> load() {
				return identifiable.getStudentSubjectEvaluations();
			}
			@Override
			public Crud getCrud() {
				return crud;
			}
			@Override
			public Boolean isShowAddButton() {
				return Boolean.FALSE;
			}
			@Override
			public void instanciated(AbstractItemCollection<StudentSubjectEvaluationItem, StudentClassroomSessionDivisionSubjectEvaluation,SelectItem> itemCollection,StudentSubjectEvaluationItem mark) {
				super.instanciated(itemCollection, mark);
				mark.setRegistrationCode(mark.getIdentifiable().getStudentSubject().getStudent().getCode());
				mark.setNames(mark.getIdentifiable().getStudentSubject().getStudent().getPerson().getNames());
				mark.setValue(mark.getIdentifiable().getValue());
				mark.setValueAsString(inject(NumberBusiness.class).format(mark.getValue()));
			}	
			@Override
			public void write(StudentSubjectEvaluationItem item) {
				super.write(item);
				item.getIdentifiable().setValue(item.getValue());
			}
		});
		((AbstractWebApplicableValueQuestion)markCollection.getApplicableValueQuestion()).setUpdate("markValue");
		markCollection.getDeleteCommandable().setRendered(Boolean.FALSE);
		markCollection.getApplicableValueQuestion().setRendered(Boolean.TRUE);
		//markCollection.getAddCommandable().setRendered(Boolean.FALSE);
		form.getControlSetListeners().add(new ControlSetAdapter<Object>(){
			private static final long serialVersionUID = 1L;

			@Override
			public Boolean build(Object data,Field field) {
				if(field.getName().equals(Form.FIELD_TYPE))
					return subjectEvaluationType == null;
				return false;
			}
		});
		
		//TODO make it in super class
		//markCollection.setShowFooter(markCollection.getAddCommandable().getRendered());
		//onDocumentLoadJavaScript = markCollection.getFormatJavaScript();
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_TYPE, inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
	}
	
	@Override
	protected void create() {
		identifiable.setStudentSubjectEvaluations(markCollection.getIdentifiables());
		super.create();
		schoolWebManager.initialiseNavigatorTree(userSession);
	}
	
	@Override
	protected void update() {
		inject(EvaluationBusiness.class).save(identifiable,markCollection.getIdentifiables());
	}
	
	@Override
	protected void delete() {
		super.delete();
		schoolWebManager.initialiseNavigatorTree(userSession);
	}
	
	@Override
	protected Boolean consultOnSuccess() {
		return Boolean.TRUE;
	}
	
	protected Evaluation instanciateIdentifiable() {
		Evaluation subjectEvaluation = inject(EvaluationBusiness.class).newInstance(classroomSessionDivisionSubject);
		subjectEvaluation.setClassroomSessionDivisionSubjectEvaluationType(subjectEvaluationType);
		return subjectEvaluation;
	}
		
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = instanciateCommandableBuilder().setLabel(formatUsingBusiness(subjectEvaluationType)).create();
	
		contextualMenu.getChildren().add(Builder.createConsult(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession(), null));
		contextualMenu.getChildren().add(Builder.createConsult(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision(), null));
		contextualMenu.getChildren().add(Builder.createConsult(classroomSessionDivisionSubject, null));
		
		return Arrays.asList(contextualMenu);
	}
		
	@Getter @Setter
	public static class Form extends AbstractFormModel<Evaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo @NotNull private ClassroomSessionDivisionSubjectEvaluationType type;
		//@Input @InputCalendar @NotNull private Date date;
		@NotNull private Boolean coefficientApplied = Boolean.TRUE;
		public static final String FIELD_TYPE = "type";
	}
	
	@Getter @Setter
	public static class StudentSubjectEvaluationItem extends AbstractItemCollectionItem<StudentClassroomSessionDivisionSubjectEvaluation> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private String registrationCode;
		private String names;
		private BigDecimal value;
		private String valueAsString;
				
		@Override
		public String toString() {
			return registrationCode+" "+names+" "+value;
		}
	}
	
	/**/
	
}
