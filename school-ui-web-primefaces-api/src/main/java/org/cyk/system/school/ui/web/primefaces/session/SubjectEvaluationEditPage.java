package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.ui.api.UIProvider;
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

@Named @ViewScoped @Getter @Setter
public class SubjectEvaluationEditPage extends AbstractCrudOnePage<SubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	private SubjectEvaluationType subjectEvaluationType;
	private ItemCollection<Mark,StudentSubjectEvaluation> markCollection;
	private BigDecimal maximumValue;
	private Integer decimalPlaces = 0;
	
	@Override
	protected void initialisation() {
		Long subjectEvaluationTypeIdentifier = requestParameterLong(SubjectEvaluationType.class);
		if(subjectEvaluationTypeIdentifier==null){
			Long classroomSessionDivisionSubjectIdentifier = requestParameterLong(ClassroomSessionDivisionSubject.class);
			if(classroomSessionDivisionSubjectIdentifier==null)
				;
			else
				classroomSessionDivisionSubject = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().find(classroomSessionDivisionSubjectIdentifier);	
		}else{
			subjectEvaluationType = SchoolBusinessLayer.getInstance().getSubjectEvaluationTypeBusiness().find(subjectEvaluationTypeIdentifier);
			classroomSessionDivisionSubject = subjectEvaluationType.getSubject();
		}
			
		super.initialisation();
		if(subjectEvaluationType!=null){
			maximumValue = identifiable.getType().getMaximumValue();
		}
		if(Crud.CREATE.equals(crud)){
			
		}else{
			identifiable.setStudentSubjectEvaluations(SchoolBusinessLayer.getInstance().getStudentSubjectEvaluationBusiness().findBySubjectEvaluation(identifiable,Crud.UPDATE.equals(crud)));
			subjectEvaluationType = identifiable.getType();
			classroomSessionDivisionSubject = subjectEvaluationType.getSubject();
		}
	
		contentTitle = formatPathUsingBusiness(ClassroomSession.class,identifiable);
		
		markCollection = createItemCollection(Mark.class, StudentSubjectEvaluation.class,identifiable.getStudentSubjectEvaluations(),new ItemCollectionWebAdapter<Mark,StudentSubjectEvaluation>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<Mark, StudentSubjectEvaluation,SelectItem> itemCollection,Mark mark) {
				super.instanciated(itemCollection, mark);
				mark.setRegistrationCode(mark.getIdentifiable().getStudentSubject().getStudent().getRegistration().getCode());
				mark.setNames(mark.getIdentifiable().getStudentSubject().getStudent().getPerson().getNames());
				mark.setValue(mark.getIdentifiable().getValue());
			}	
			@Override
			public void write(Mark item) {
				super.write(item);
				item.getIdentifiable().setValue(item.getValue());
			}
		});
		((AbstractWebApplicableValueQuestion)markCollection.getApplicableValueQuestion()).setUpdate("markValue");
		markCollection.getDeleteCommandable().setRendered(Boolean.FALSE);
		markCollection.getApplicableValueQuestion().setRendered(Boolean.TRUE);
		markCollection.getAddCommandable().setRendered(Boolean.FALSE);
		form.getControlSetListeners().add(new ControlSetAdapter<Object>(){
			@Override
			public Boolean build(Field field) {
				if(field.getName().equals(Form.FIELD_TYPE))
					return subjectEvaluationType == null;
				return false;
			}
		});
	}
	
	@Override
	protected void afterInitialisation() {
		super.afterInitialisation();
		setChoices(Form.FIELD_TYPE, SchoolBusinessLayer.getInstance().getSubjectEvaluationTypeBusiness().findByClassroomSessionDivisionSubject(classroomSessionDivisionSubject));
	}
	
	@Override
	protected void create() {
		identifiable.setStudentSubjectEvaluations(markCollection.getIdentifiables());
		super.create();
	}
	
	@Override
	protected void update() {
		SchoolBusinessLayer.getInstance().getSubjectEvaluationBusiness().save(identifiable,markCollection.getIdentifiables());
	}
	
	@Override
	protected Boolean consultOnSuccess() {
		return Boolean.TRUE;
	}
	
	protected SubjectEvaluation instanciateIdentifiable() {
		SubjectEvaluation subjectEvaluation = SchoolBusinessLayer.getInstance().getSubjectEvaluationBusiness().newInstance(classroomSessionDivisionSubject);
		subjectEvaluation.setType(subjectEvaluationType);
		return subjectEvaluation;
	}
		
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	protected Collection<UICommandable> contextualCommandables() {
		UICommandable contextualMenu = UIProvider.getInstance().createCommandable("button", null);
		contextualMenu.setLabel(formatUsingBusiness(subjectEvaluationType)); 
		
		contextualMenu.getChildren().add(navigationManager.createConsultCommandable(identifiable.getType().getSubject().getClassroomSessionDivision().getClassroomSession(), null));
		contextualMenu.getChildren().add(navigationManager.createConsultCommandable(identifiable.getType().getSubject().getClassroomSessionDivision(), null));
		contextualMenu.getChildren().add(navigationManager.createConsultCommandable(classroomSessionDivisionSubject, null));
		
		return Arrays.asList(contextualMenu);
	}
		
	@Getter @Setter
	public static class Form extends AbstractFormModel<SubjectEvaluation> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		@Input @InputChoice(load=false) @InputOneChoice @InputOneCombo @NotNull private SubjectEvaluationType type;
		//@Input @InputCalendar @NotNull private Date date;
		@NotNull private Boolean coefficientApplied = Boolean.TRUE;
		public static final String FIELD_TYPE = "type";
	}
	
	@Getter @Setter
	public static class Mark extends AbstractItemCollectionItem<StudentSubjectEvaluation> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private String registrationCode;
		private String names;
		private BigDecimal value;
				
		@Override
		public String toString() {
			return registrationCode+" "+names+" "+value;
		}
	}

}
