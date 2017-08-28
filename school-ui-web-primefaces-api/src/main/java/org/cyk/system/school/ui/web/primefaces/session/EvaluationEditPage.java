package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Date;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectEvaluationBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.system.school.ui.web.primefaces.session.student.StudentClassroomSessionEditSubjectsPage.Form;
import org.cyk.ui.api.command.menu.AbstractSystemMenuBuilder;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.AbstractWebApplicableValueQuestion;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputCalendar;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class EvaluationEditPage extends AbstractCrudOnePage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	private ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType;
	private ItemCollection<StudentClassroomSessionDivisionSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation,Evaluation> studentClassroomSessionDivisionSubjectEvaluationCollection;
	private BigDecimal maximumValue;
	private String maximumValueAsString;
	private Integer decimalPlaces = 0;
	
	@Override
	protected void initialisation() {
		classroomSessionDivisionSubjectEvaluationType = webManager.getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubjectEvaluationType.class, Boolean.TRUE);
		if(classroomSessionDivisionSubjectEvaluationType==null)
			classroomSessionDivisionSubject = webManager.getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubject.class, Boolean.TRUE);
		else
			classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
		
		super.initialisation();
		/*if(classroomSessionDivisionSubjectEvaluationType==null){
			maximumValue = new BigDecimal("10");
		}else{*/
			maximumValue = identifiable.getClassroomSessionDivisionSubjectEvaluationType().getMaximumValue();
		//}
		maximumValueAsString = inject(NumberBusiness.class).format(maximumValue);
		if(Crud.CREATE.equals(crud)){
			
		}else{
			identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().setCollection(inject(StudentClassroomSessionDivisionSubjectEvaluationBusiness.class).findByEvaluation(identifiable,Crud.UPDATE.equals(crud)));
			classroomSessionDivisionSubjectEvaluationType = identifiable.getClassroomSessionDivisionSubjectEvaluationType();
			classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
		}
		studentClassroomSessionDivisionSubjectEvaluationCollection = createItemCollection(StudentClassroomSessionDivisionSubjectEvaluationItem.class, StudentClassroomSessionDivisionSubjectEvaluation.class,identifiable
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<StudentClassroomSessionDivisionSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation,Evaluation>(identifiable,crud,form,StudentClassroomSessionDivisionSubjectEvaluation.class){
			private static final long serialVersionUID = -3872058204105902514L;
			/*@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> create() {
				return identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection();
			}
			/*@Override
			public Collection<StudentClassroomSessionDivisionSubjectEvaluation> load() {
				return identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().getCollection();
			}
			@Override
			public Boolean isShowAddButton() {
				return Boolean.FALSE;
			}
			
			@Override
			public IdentifiableRuntimeCollection<StudentClassroomSessionDivisionSubjectEvaluation> getRuntimeCollection() {
				return identifiable.getStudentClassroomSessionDivisionSubjectEvaluations();
			}*/
			
			@Override
			public void instanciated(AbstractItemCollection<StudentClassroomSessionDivisionSubjectEvaluationItem, StudentClassroomSessionDivisionSubjectEvaluation,Evaluation,SelectItem> itemCollection,StudentClassroomSessionDivisionSubjectEvaluationItem item) {
				super.instanciated(itemCollection, item);
				item.setStudent(item.getIdentifiable().getStudentClassroomSessionDivisionSubject().getStudent().getCode()+Constant.CHARACTER_SPACE
						+item.getIdentifiable().getStudentClassroomSessionDivisionSubject().getStudent().getPerson().getNames());
				item.setValue(item.getIdentifiable().getValue());
				item.setValueAsString(inject(NumberBusiness.class).format(item.getValue()));
			}	
			@Override
			public void write(StudentClassroomSessionDivisionSubjectEvaluationItem item) {
				super.write(item);
				item.getIdentifiable().setValue(item.getValue());
			}
			
			@Override
			public String getFieldOneItemMasterSelectedName() {
				return Form.FIELD_ONE_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_SELECTED;
			}
			
		});
		((AbstractWebApplicableValueQuestion)studentClassroomSessionDivisionSubjectEvaluationCollection.getApplicableValueQuestion()).setUpdate("markValue");
		studentClassroomSessionDivisionSubjectEvaluationCollection.getDeleteCommandable().setRendered(Boolean.FALSE);
		studentClassroomSessionDivisionSubjectEvaluationCollection.getApplicableValueQuestion().setRendered(Boolean.TRUE);
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
		
		//identifiable.getStudentClassroomSessionDivisionSubjectEvaluations().setSynchonizationEnabled(Boolean.TRUE);
		
		
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_TYPE, inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
		
		@SuppressWarnings("unchecked")
		org.cyk.ui.api.data.collector.control.InputChoice<?, ?, ?, ?, ?, SelectItem> input = (org.cyk.ui.api.data.collector.control.InputChoice<?, ?, ?, ?, ?, SelectItem>)
				form.getInputByFieldName(Form.FIELD_ONE_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_SELECTED);
		/*
		System.out.println(inject(StudentClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionSubject(identifiable
						.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject()));
		
		System.out.println(input.getList());
		
		input.getList().addAll(WebManager.getInstance().getSelectItems(StudentClassroomSessionDivisionSubject.class
				,inject(StudentClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionSubject(identifiable
						.getClassroomSessionDivisionSubjectEvaluationType().getClassroomSessionDivisionSubject()),Boolean.FALSE));
		*/
	}
	
	/*@Override
	public void transfer(UICommand command, Object parameter) throws Exception {
		super.transfer(command, parameter);
		if(form.getSubmitCommandable().getCommand()==command){
			getIdentifiable().getStudentClassroomSessionDivisionSubjectEvaluations().setCollection(studentClassroomSessionDivisionSubjectEvaluationCollection.getIdentifiables());
		}
	}*/
	
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
		Evaluation evaluation = classroomSessionDivisionSubjectEvaluationType == null 
				? inject(EvaluationBusiness.class).instanciateOne(classroomSessionDivisionSubject,Boolean.TRUE)
				: inject(EvaluationBusiness.class).instanciateOne(classroomSessionDivisionSubjectEvaluationType,Boolean.TRUE);
		return evaluation;
	}
			
	@Getter @Setter
	public static class Form extends AbstractFormModel<Evaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input(readOnly=true,disabled=true) @InputChoice(load=false) @InputOneChoice @InputOneCombo @NotNull private ClassroomSessionDivisionSubjectEvaluationType type;
		@Input @InputCalendar @NotNull private Date date;
		@NotNull private Boolean coefficientApplied = Boolean.TRUE;
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false,load=false) @InputOneChoice @InputOneCombo private Student oneStudentClassroomSessionDivisionSubjectEvaluationSelected;
		
		public static final String FIELD_TYPE = "type";
		public static final String FIELD_DATE = "date";
		public static final String FIELD_COEFFICIENT_APPLIED = "coefficientApplied";
		
		public static final String FIELD_ONE_STUDENT_CLASSROOM_SESSION_DIVISION_SUBJECT_EVALUATION_SELECTED = "oneStudentClassroomSessionDivisionSubjectEvaluationSelected";
	
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
