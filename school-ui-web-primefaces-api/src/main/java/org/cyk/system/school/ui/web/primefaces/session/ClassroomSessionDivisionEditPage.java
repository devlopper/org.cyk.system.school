package org.cyk.system.school.ui.web.primefaces.session;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Date;

import javax.faces.model.SelectItem;
import javax.faces.view.ViewScoped;
import javax.inject.Named;

import org.cyk.system.root.business.api.language.LanguageBusiness;
import org.cyk.system.school.business.api.session.ClassroomSessionBusiness;
import org.cyk.system.school.model.session.ClassroomSessionDivision;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.api.data.collector.form.ControlSet;
import org.cyk.ui.web.primefaces.data.collector.control.ControlSetAdapter;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputCalendar;
import org.cyk.utility.common.annotation.user.interfaces.InputNumber;
import org.primefaces.extensions.model.dynaform.DynaFormControl;
import org.primefaces.extensions.model.dynaform.DynaFormLabel;
import org.primefaces.extensions.model.dynaform.DynaFormModel;
import org.primefaces.extensions.model.dynaform.DynaFormRow;

import lombok.Getter;
import lombok.Setter;

@Named @ViewScoped @Getter @Setter
public class ClassroomSessionDivisionEditPage extends AbstractCrudOnePage<ClassroomSessionDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
	
	@Override
	protected void initialisation() {
		super.initialisation();
		form.getControlSetListeners().add(new ControlSetAdapter<Object>(){
			private static final long serialVersionUID = 1L;

			@Override
			public String fiedLabel(ControlSet<Object, DynaFormModel, DynaFormRow, DynaFormLabel, DynaFormControl, SelectItem> controlSet,Field field) {
				if(field.getName().equals(Form.FIELD_DURATION))
					return inject(LanguageBusiness.class).findText("field.number.of",new Object[]{
							inject(ClassroomSessionBusiness.class).findCommonNodeInformations(identifiable.getClassroomSession())
							.getAttendanceTimeDivisionType().getName()
					});
				
				return super.fiedLabel(controlSet, field);
			}
		});
	}
	
	public static class Form extends AbstractFormModel<ClassroomSessionDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputCalendar private Date fromDate;
		@Input @InputCalendar private Date toDate;
		@Input @InputNumber private BigDecimal duration;
		
		@Override
		public void read() {
			super.read();
			if(identifiable.getNumberOfMillisecond()==null)
				;
			else
				duration = inject(ClassroomSessionBusiness.class).convertAttendanceTimeToDivisionDuration(identifiable.getClassroomSession(),identifiable.getNumberOfMillisecond());
			
			fromDate = identifiable.getExistencePeriod().getFromDate();
			toDate = identifiable.getExistencePeriod().getToDate();
		}
		
		@Override
		public void write() {
			super.write();
			if(duration==null)
				identifiable.setNumberOfMillisecond(null);
			else
				identifiable.setNumberOfMillisecond(inject(ClassroomSessionBusiness.class).convertAttendanceTimeToMillisecond(identifiable.getClassroomSession(),duration));
			
			identifiable.getExistencePeriod().setFromDate(fromDate);
			identifiable.getExistencePeriod().setToDate(toDate);
		}
		
		public static final String FIELD_DURATION = "duration";
			
	}

}
