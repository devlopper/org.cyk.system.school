package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;

import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyk.system.root.business.api.Crud;
import org.cyk.system.school.business.impl.SchoolBusinessLayer;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.StudentSubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluation;
import org.cyk.system.school.model.subject.SubjectEvaluationType;
import org.cyk.ui.api.command.UICommand;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.model.AbstractItemCollection;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.api.model.ItemCollectionListener.ItemCollectionAdapter;
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
public class SubjectEvaluationEditPage extends AbstractCrudOnePage<SubjectEvaluation> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	private ClassroomSessionDivisionSubject classroomSessionDivisionSubject;
	private SubjectEvaluationType subjectEvaluationType;
	private ItemCollection<Mark,StudentSubjectEvaluation> markCollection;
	private BigDecimal maximumValue;
	private Integer decimalPlaces = 0;
	
	@Override
	protected void initialisation() {
		Long classroomSessionDivisionSubjectIdentifier = requestParameterLong(uiManager.businessEntityInfos(ClassroomSessionDivisionSubject.class).getIdentifier());
		if(classroomSessionDivisionSubjectIdentifier!=null)
			classroomSessionDivisionSubject = SchoolBusinessLayer.getInstance().getClassroomSessionDivisionSubjectBusiness().find(classroomSessionDivisionSubjectIdentifier);
		
		Long subjectEvaluationTypeIdentifier = requestParameterLong(uiManager.businessEntityInfos(SubjectEvaluationType.class).getIdentifier());
		if(subjectEvaluationTypeIdentifier!=null)
			subjectEvaluationType = SchoolBusinessLayer.getInstance().getSubjectEvaluationTypeBusiness().find(subjectEvaluationTypeIdentifier);
		
		super.initialisation();
		identifiable.setType(subjectEvaluationType);
		if(identifiable.getType()!=null){
			maximumValue = identifiable.getType().getMaximumValue();
		}
		contentTitle = formatUsingBusiness(new Object[]{identifiable.getType().getSubject().getClassroomSessionDivision().getClassroomSession(),
				identifiable.getType().getSubject().getClassroomSessionDivision(),identifiable.getType().getSubject(),identifiable.getType()});
		if(Crud.CREATE.equals(crud)){
			
		}else
			classroomSessionDivisionSubject = identifiable.getType().getSubject();
		
		markCollection = createItemCollection(form, "qwerty", Mark.class, StudentSubjectEvaluation.class, identifiable.getStudentSubjectEvaluations(),new ItemCollectionAdapter<Mark,StudentSubjectEvaluation>(){
			private static final long serialVersionUID = -3872058204105902514L;
			@Override
			public void instanciated(AbstractItemCollection<Mark, StudentSubjectEvaluation> itemCollection,Mark mark) {
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
			@Override
			public void delete(AbstractItemCollection<Mark, StudentSubjectEvaluation> itemCollection, Mark item) {
				markCollection.write();
				super.delete(itemCollection, item);
				debug(itemCollection.getItems().get(0));
			}
		});
		//markCollection.getDeleteCommandable().setRendered(Boolean.FALSE);
		//markCollection.setShowApplicable(Boolean.TRUE);
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
	protected void update() {
		SchoolBusinessLayer.getInstance().getSubjectEvaluationBusiness().save(identifiable,identifiable.getStudentSubjectEvaluations());
	}
	
	protected SubjectEvaluation instanciateIdentifiable() {
		return SchoolBusinessLayer.getInstance().getSubjectEvaluationBusiness()
				.newInstance(subjectEvaluationType==null?classroomSessionDivisionSubject:subjectEvaluationType.getSubject());
	}
		
	@Override
	protected Class<?> __formModelClass__() {
		return Form.class;
	}
	
	@Override
	public void transfer(UICommand arg0, Object arg1) throws Exception {
		super.transfer(arg0, arg1);
		System.out.println("SubjectEvaluationEditPage.transfer()");
		markCollection.write();
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
