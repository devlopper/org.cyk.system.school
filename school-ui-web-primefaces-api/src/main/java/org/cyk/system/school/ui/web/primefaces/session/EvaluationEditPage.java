package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Date;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.SchoolWebManager;
import org.cyk.ui.api.command.UICommand;
import org.cyk.ui.api.command.menu.AbstractSystemMenuBuilder;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.AbstractWebApplicableValueQuestion;
import org.cyk.ui.web.api.ItemCollectionWebAdapter;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputCalendar;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class EvaluationEditPage extends AbstractCrudOnePage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	private ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType;
	private ItemCollection<StudentClassroomSessionDivisionSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation,Evaluation> markCollection;
	private BigDecimal maximumValue;
	private Integer decimalPlaces = 0;
	
	@Inject private SchoolWebManager schoolWebManager;
	
	@Override
	protected void initialisation() {
		classroomSessionDivisionSubjectEvaluationType = webManager.getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubjectEvaluationType.class, Boolean.TRUE);
		if(classroomSessionDivisionSubjectEvaluationType==null)
			classroomSessionDivisionSubject = webManager.getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubject.class, Boolean.TRUE);
		else
			classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
		
		super.initialisation();
		if(classroomSessionDivisionSubjectEvaluationType!=null){
			maximumValue = identifiable.getClassroomSessionDivisionSubjectEvaluationType().getMaximumValue();
		}
		if(Crud.CREATE.equals(crud)){
			
		}else{
			identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().setCollection(inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).findByEvaluation(identifiable,Crud.UPDATE.equals(crud)));
			classroomSessionDivisionSubjectEvaluationType = identifiable.getClassroomSessionDivisionSubjectEvaluationType();
			classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
		}
		
		markCollection = createItemCollection(StudentClassroomSessionDivisionSubjectEvaluationItem.class, StudentClassroomSessionDivisionSubjectEvaluation.class,identifiable,new ItemCollectionWebAdapter<StudentClassroomSessionDivisionSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation,Evaluation>(identifiable,crud){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> create() {
				return identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection();
			}
			@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> load() {
				return identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection();
			}
			@Override
			public Boolean isShowAddButton() {
				return Boolean.FALSE;
			}
			@Override
			public void instanciated(AbstractItemCollection<StudentClassroomSessionDivisionSubjectEvaluationItem, StudentClassroomSessionDivisionSubjectEvaluation,Evaluation,SelectItem> itemCollection,StudentClassroomSessionDivisionSubjectEvaluationItem mark) {
				super.instanciated(itemCollection, mark);
				mark.setStudent(mark.getIdentifiable().getStudentClassroomSessionDivisionSubject().getStudent().getCode()+Constant.CHARACTER_SPACE
						+mark.getIdentifiable().getStudentClassroomSessionDivisionSubject().getStudent().getPerson().getNames());
				mark.setValue(mark.getIdentifiable().getValue());
				mark.setValueAsString(inject(NumberBusiness.class).format(mark.getValue()));
			}	
			@Override
			public void write(StudentClassroomSessionDivisionSubjectEvaluationItem item) {
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
				if(field.getName().equals(EvaluationEditPage.Form.FIELD_TYPE))
					return classroomSessionDivisionSubjectEvaluationType == null;
				return false;
			}
		});
		
		//TODO make it in super class
		//markCollection.setShowFooter(markCollection.getAddCommandable().getRendered());
		//onDocumentLoadJavaScript = markCollection.getFormatJavaScript();
		
		identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().setSynchonizationEnabled(Boolean.TRUE);
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_TYPE, inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
	}
	
	@Override
	public void transfer(UICommand command, Object parameter) throws Exception {
		super.transfer(command, parameter);
		if(form.getSubmitCommandable().getCommand()==command){
			getIdentifiable().getStudentClassroomSessionDivisionSubjectEvaluations().setCollection(markCollection.getIdentifiables() /*itemCollection.getIdentifiables()*/);
		}
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void create() {
		//identifiable.setStudentSubjectEvaluations(markCollection.getIdentifiables());
		super.create();
		//schoolWebManager.initialiseNavigatorTree(userSession);
		AbstractSystemMenuBuilder.DEFAULT.initialiseNavigatorTree(userSession);//TODO do it well
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void delete() {
		super.delete();
		//schoolWebManager.initialiseNavigatorTree(userSession);
		AbstractSystemMenuBuilder.DEFAULT.initialiseNavigatorTree(userSession);//TODO do it well
	}
	
	@Override
	protected Boolean consultOnSuccess() {
		return Boolean.TRUE;
	}
	
	protected Evaluation instanciateIdentifiable() {
		Evaluation evaluation = inject(EvaluationBusiness.class).instanciateOne(classroomSessionDivisionSubject);
		evaluation.setClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType);
		return evaluation;
	}
		
	/*
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = instanciateCommandableBuilder().setLabel(formatUsingBusiness(classroomSessionDivisionSubjectEvaluationType)).create();
	
		contextualMenu.getChildren().add(Builder.createConsult(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision().getClassroomSession(), null));
		contextualMenu.getChildren().add(Builder.createConsult(identifiable.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject().getClassroomSessionDivision(), null));
		contextualMenu.getChildren().add(Builder.createConsult(classroomSessionDivisionSubject, null));
		
		return Arrays.asList(contextualMenu);
	}*/
		
	@Getter @Setter
	public static class Form extends AbstractFormModel<Evaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input(readOnly=true,disabled=true) @InputChoice(load=false) @InputOneChoice @InputOneCombo @NotNull private ClassroomSessionDivisionSubjectEvaluationType type;
		@Input @InputCalendar @NotNull private Date date;
		@NotNull private Boolean coefficientApplied = Boolean.TRUE;
		
		public static final String FIELD_TYPE = "type";
		public static final String FIELD_DATE = "date";
		public static final String FIELD_COEFFICIENT_APPLIED = "coefficientApplied";
	
	}
	
	@Getter @Setter
	public static class StudentClassroomSessionDivisionSubjectEvaluationItem extends AbstractItemCollectionItem<StudentClassroomSessionDivisionSubjectEvaluation> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
	
		private String student;
		private BigDecimal value;
		private String valueAsString;
				
	}
	
	/**/
	
}
