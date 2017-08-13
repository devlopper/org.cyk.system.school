package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.actor.Student;
import org.cyk.system.school.model.session.ClassroomSession;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.system.school.model.subject.Subject;
import org.cyk.ui.api.model.AbstractBusinessIdentifiedEditFormModel;
import org.cyk.ui.api.model.time.PeriodFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;
import org.cyk.utility.common.annotation.user.interfaces.Input.RendererStrategy;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionEditPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;

	@Override
	protected void initialisation() {
		super.initialisation();
		
		/*
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
		*/
	}
	
	@Override
	protected ClassroomSessionDivision instanciateIdentifiable() {
		ClassroomSessionDivision classroomSessionDivision =  super.instanciateIdentifiable();
		classroomSessionDivision.setClassroomSession(webManager.getIdentifiableFromRequestParameter(ClassroomSession.class, Boolean.TRUE));
		return classroomSessionDivision;
	}

	public static class Form extends AbstractBusinessIdentifiedEditFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo private ClassroomSession classroomSession;
		@Input @InputChoice @InputOneChoice @InputOneCombo private TimeDivisionType timeDivisionType;
		
		//TODO should not be here
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Subject oneSubjectSelected;
		@Input(rendererStrategy=RendererStrategy.MANUAL) @InputChoice(nullable=false) @InputOneChoice @InputOneCombo protected Student oneStudentSelected;
		
		{ 
			existencePeriod.setSetListener(new PeriodFormModel.SetListener() {
				@Override
				public BigDecimal convertFromMillisecond(Long value) {
					return inject(ClassroomSessionBusiness.class).convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSession(),identifiable.getExistencePeriod().getNumberOfMillisecond().get());
				}
			});
			
			existencePeriod.setWriteListener(new PeriodFormModel.WriteListener() {
				@Override
				public Long convertToMillisecond(BigDecimal value) {
					return inject(ClassroomSessionBusiness.class).convertAttendanceTimeToMillisecond(identifiable.getClassroomSession(),value);
				}
			});
		}
		
		public static final String FIELD_CLASSROOM_SESSION = "classroomSession";
		public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
		public static final String FIELD_ONE_SUBJECT_SELECTED = "oneSubjectSelected";
		public static final String FIELD_ONE_STUDENT_SELECTED = "oneStudentSelected";
			
	}

}
