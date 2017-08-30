package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.business.api.mathematics.NumberBusiness;
import org.cyk.system.school.business.api.subject.ClassroomSessionDivisionSubjectEvaluationTypeBusiness;
import org.cyk.system.school.business.api.subject.EvaluationBusiness;
import org.cyk.system.school.business.api.subject.StudentClassroomSessionDivisionSubjectBusiness;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubjectEvaluationType;
import org.cyk.system.school.model.subject.Evaluation;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentClassroomSessionDivisionSubjectEvaluation;
import org.cyk.ui.api.command.menu.AbstractSystemMenuBuilder;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.api.AbstractWebApplicableValueQuestion;
import org.cyk.ui.web.api.SelectItemHelper;
import org.cyk.ui.web.api.WebManager;
import org.cyk.ui.web.primefaces.Commandable;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.Constant;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.helper.InstanceHelper;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class EvaluationEditPage extends AbstractCrudOnePage<Evaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	private ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType;
	private ItemCollection<StudentClassroomSessionDivisionSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation,Evaluation> studentClassroomSessionDivisionSubjectEvaluationCollection;

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
		if(classroomSessionDivisionSubjectEvaluationType==null)
			classroomSessionDivisionSubjectEvaluationType = identifiable.getClassroomSessionDivisionSubjectEvaluationType();
		
		selectClassroomSessionDivisionSubjectEvaluationType(classroomSessionDivisionSubjectEvaluationType);
		
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		
		studentClassroomSessionDivisionSubjectEvaluationCollection = createItemCollection(StudentClassroomSessionDivisionSubjectEvaluationItem.class, StudentClassroomSessionDivisionSubjectEvaluation.class,identifiable
				,new org.cyk.ui.web.primefaces.ItemCollectionAdapter<StudentClassroomSessionDivisionSubjectEvaluationItem,StudentClassroomSessionDivisionSubjectEvaluation,Evaluation>(identifiable,crud,form,StudentClassroomSessionDivisionSubjectEvaluation.class){
			private static final long serialVersionUID = -3872058204105902514L;
			
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
		
		//((Commandable)studentClassroomSessionDivisionSubjectEvaluationCollection.getDeleteCommandable()).getButton().setImmediate(Boolean.TRUE);
		studentClassroomSessionDivisionSubjectEvaluationCollection.getDeleteCommandable().setSkipValidation(Boolean.TRUE);
	}
	
	private void selectClassroomSessionDivisionSubjectEvaluationType(ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType){
		maximumValueAsString = inject(NumberBusiness.class).format(classroomSessionDivisionSubjectEvaluationType.getMaximumValue());
		classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
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
		Evaluation evaluation = classroomSessionDivisionSubjectEvaluationType == null 
				? inject(EvaluationBusiness.class).instanciateOne(classroomSessionDivisionSubject,Boolean.TRUE)
				: inject(EvaluationBusiness.class).instanciateOne(classroomSessionDivisionSubjectEvaluationType,Boolean.TRUE);
		return evaluation;
	}
			
	@Getter @Setter
	public static class Form extends AbstractFormModel<Evaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input(readOnly=true,disabled=true) 
		@InputChoice(itemBuilderClass=ClassroomSessionDivisionSubjectEvaluationTypeSelectItemBuilder.class,getChoicesClass=ClassroomSessionDivisionSubjectEvaluationTypes.class)
		@InputOneChoice @InputOneCombo @NotNull 
		private ClassroomSessionDivisionSubjectEvaluationType type;
		
		@NotNull private Boolean coefficientApplied = Boolean.TRUE;
		
		@Input(rendererStrategy=RendererStrategy.MANUAL) 
		@InputChoice(itemBuilderClass=StudentClassroomSessionDivisionSubjectSelectItemBuilder.class,getChoicesClass=StudentClassroomSessionDivisionSubjects.class,nullable=false) 
		@InputOneChoice @InputOneCombo 
		private StudentClassroomSessionDivisionSubject oneStudentClassroomSessionDivisionSubjectEvaluationSelected;
		
		@Override
		public void read() {
			super.read();
			type = identifiable.getClassroomSessionDivisionSubjectEvaluationType();
		}
		
		public static final String FIELD_TYPE = "type";
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
	
	public static class ClassroomSessionDivisionSubjectEvaluationTypeSelectItemBuilder extends SelectItemHelper.OneBuilder {
		private static final long serialVersionUID = 1L;
		@Override
		public String getFieldValue(Object instance, String fieldName) {
			if(instance instanceof ClassroomSessionDivisionSubjectEvaluationType){
				ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = (ClassroomSessionDivisionSubjectEvaluationType) instance;
				return String.format(FORMAT, classroomSessionDivisionSubjectEvaluationType.getEvaluationType().getName(),classroomSessionDivisionSubjectEvaluationType.getMaximumValue());
			}
				
			return super.getFieldValue(instance, fieldName);
		}
		
		public static String FORMAT = "%s/%s";
	}
	
	public static class ClassroomSessionDivisionSubjectEvaluationTypes extends InstanceHelper.Many.Adapter.Default implements Serializable {
		private static final long serialVersionUID = 1L;

		@Override
		protected Collection<?> __execute__() {
			Evaluation evaluation = WebManager.getInstance().getIdentifiableFromRequestParameter(Evaluation.class, Boolean.FALSE);
			ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = evaluation == null
					?  WebManager.getInstance().getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubjectEvaluationType.class, Boolean.TRUE)
					: evaluation.getClassroomSessionDivisionSubjectEvaluationType();
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType == null ? WebManager.getInstance()
					.getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubject.class, Boolean.TRUE) 
					: classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
			return inject(ClassroomSessionDivisionSubjectEvaluationTypeBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
		}
		
	}
	
	public static class StudentClassroomSessionDivisionSubjectSelectItemBuilder extends SelectItemHelper.OneBuilder {
		private static final long serialVersionUID = 1L;
		@Override
		public String getFieldValue(Object instance, String fieldName) {
			if(instance instanceof StudentClassroomSessionDivisionSubject){
				StudentClassroomSessionDivisionSubject studentClassroomSessionDivisionSubject = (StudentClassroomSessionDivisionSubject) instance;
				return String.format(FORMAT, studentClassroomSessionDivisionSubject.getStudent());
			}
				
			return super.getFieldValue(instance, fieldName);
		}
		
		public static String FORMAT = "%s";
	}
	
	public static class StudentClassroomSessionDivisionSubjects extends InstanceHelper.Many.Adapter.Default implements Serializable {
		private static final long serialVersionUID = 1L;

		@Override
		protected Collection<?> __execute__() {
			Evaluation evaluation = WebManager.getInstance().getIdentifiableFromRequestParameter(Evaluation.class, Boolean.FALSE);
			ClassroomSessionDivisionSubjectEvaluationType classroomSessionDivisionSubjectEvaluationType = evaluation == null
					?  WebManager.getInstance().getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubjectEvaluationType.class, Boolean.TRUE)
					: evaluation.getClassroomSessionDivisionSubjectEvaluationType();
			ClassroomSessionDivisionSubject classroomSessionDivisionSubject = classroomSessionDivisionSubjectEvaluationType == null ? WebManager.getInstance()
					.getIdentifiableFromRequestParameter(ClassroomSessionDivisionSubject.class, Boolean.TRUE) 
					: classroomSessionDivisionSubjectEvaluationType.getClassroomSessionDivisionSubject();
			return inject(StudentClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject);
		}
		
	}
	
	/**/
	
}
