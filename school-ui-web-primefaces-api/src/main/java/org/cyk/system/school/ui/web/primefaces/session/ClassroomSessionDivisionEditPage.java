package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Date;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Teacher;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.ClassroomSessionDivisionSubject;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.data.collector.form.ControlSet;
import org.cyk.ui.api.model.AbstractItemCollectionItem;
import org.cyk.ui.web.primefaces.ItemCollection;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputCalendar;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.primefaces.extensions.model.dynaform.DynaFormControl;
import org.primefaces.extensions.model.dynaform.DynaFormLabel;
import org.primefaces.extensions.model.dynaform.DynaFormModel;
import org.primefaces.extensions.model.dynaform.DynaFormRow;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionEditPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;

	private ItemCollection<ClassroomSessionDivisionSubjectItem,ClassroomSessionDivisionSubject> classroomSessionDivisionSubjectCollection;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		/*
		classroomSessionDivisionSubjectCollection = createItemCollection(ClassroomSessionDivisionSubjectItem.class, ClassroomSessionDivisionSubject.class,new ItemCollectionWebAdapter<ClassroomSessionDivisionSubjectItem,ClassroomSessionDivisionSubject>(){
			private static final long serialVersionUID = -3872058204105902514L;
			
			@Override
			public Collection<ClassroomSessionDivisionSubject> load() {
				return inject(ClassroomSessionDivisionSubjectBusiness.class).findByClassroomSessionDivision(identifiable);
			}
			@Override
			public Crud getCrud() {
				return crud;
			}
			@Override
			public Boolean isShowAddButton() {
				return Boolean.TRUE;
			}
			
			@Override
			public void instanciated(AbstractItemCollection<ClassroomSessionDivisionSubjectItem, ClassroomSessionDivisionSubject,SelectItem> collection,ClassroomSessionDivisionSubjectItem item) {
				super.instanciated(collection, item);
				item.setSubject(item.getIdentifiable().getSubject());
				item.setTeacher(item.getIdentifiable().getTeacher());
				item.setCoefficient(item.getIdentifiable().getWeight());
			}	
			@Override
			public void write(ClassroomSessionDivisionSubjectItem item) {
				super.write(item);
				item.getIdentifiable().setSubject(item.getSubject());
				item.getIdentifiable().setTeacher(item.getTeacher());
				item.getIdentifiable().setWeight(item.getCoefficient());
			}
		});
		classroomSessionDivisionSubjectCollection.getDeleteCommandable().setRendered(Boolean.TRUE);
		classroomSessionDivisionSubjectCollection.getApplicableValueQuestion().setRendered(Boolean.FALSE);
		*/
		form.getControlSetListeners().add(new ControlSetAdapter<Object>(){
			private static final long serialVersionUID = 1L;

			@Override
			public String fiedLabel(ControlSet<Object, DynaFormModel, DynaFormRow, DynaFormLabel, DynaFormControl, SelectItem> controlSet,Object data,Field field) {
				if(field.getName().equals(ClassroomSessionDivisionEditPage.Form.FIELD_DURATION))
					return inject(LanguageBusiness.class).findText("field.number.of",new Object[]{
							inject(ClassroomSessionBusiness.class).findCommonNodeInformations(identifiable.getClassroomSession())
							.getAttendanceTimeDivisionType().getName()
					});
				
				return super.fiedLabel(controlSet,data, field);
			}
		});
	}

	public static class Form extends AbstractFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		//TODO use PeriodFormModel instead
		@Input @InputCalendar private Date fromDate;
		@Input @InputCalendar private Date toDate;
		@Input @InputNumber private BigDecimal duration;
		
		@Override
		public void read() {
			super.read();
			fromDate = identifiable.getExistencePeriod().getFromDate();
			toDate = identifiable.getExistencePeriod().getToDate();
			if(identifiable.getExistencePeriod().getNumberOfMillisecond()!=null)
				duration = inject(ClassroomSessionBusiness.class).convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSession(),identifiable.getExistencePeriod().getNumberOfMillisecond().get());
		}
		
		@Override
		public void write() {
			super.write();
			identifiable.getExistencePeriod().setFromDate(fromDate);
			identifiable.getExistencePeriod().setToDate(toDate);
			if(duration==null)
				identifiable.getExistencePeriod().getNumberOfMillisecond().set(null);
			else
				identifiable.getExistencePeriod().getNumberOfMillisecond().set(
						inject(ClassroomSessionBusiness.class).convertAttendanceTimeToMillisecond(identifiable.getClassroomSession(),duration));
		}
		
		public static final String FIELD_FROM_DATE = "fromDate";
		public static final String FIELD_TO_DATE = "toDate";
		public static final String FIELD_DURATION = "duration";
			
	}
	
	@Getter @Setter
	public static class ClassroomSessionDivisionSubjectItem extends AbstractItemCollectionItem<ClassroomSessionDivisionSubject> implements Serializable {
		private static final long serialVersionUID = 3828481396841243726L;
		private Subject subject;
		private Teacher teacher;
		private BigDecimal coefficient;
				
	}

}
