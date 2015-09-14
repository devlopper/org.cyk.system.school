package org.cyk.system.school.ui.web.primefaces.registration;

import java.io.Serializable;
import java.util.Date;

import javax.enterprise.context.RequestScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.actor.Student;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputText;

@Named @RequestScoped @Getter
public class StudentRegistrationPage extends AbstractCrudOnePage<Student> implements Serializable {

	private static final long serialVersionUID = -7933944550331049645L;

	@Override
	protected Class<? extends AbstractFormModel<?>> __formModelClass__() {
		return FormModel.class;
	}
	
	/**/
	
	@Getter @Setter
	public class FormModel extends AbstractFormModel<Student> implements Serializable{

		private static final long serialVersionUID = 2063899765621101400L;
		
		@Input @InputText @NotNull
		private Date registrationDate;
		
		@Input @InputText @NotNull
		private String registrationCode;
		
		
		
	} 
	
}
