package org.cyk.system.school.ui.web.primefaces.level;

import java.io.Serializable;

import javax.enterprise.context.RequestScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.school.model.session.LevelName;
import org.cyk.ui.api.data.collector.form.AbstractFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputEditor;
import org.cyk.utility.common.annotation.user.interfaces.InputText;
import org.cyk.utility.common.annotation.user.interfaces.InputTextarea;
import org.cyk.utility.common.annotation.user.interfaces.OutputSeperator;
import org.cyk.utility.common.annotation.user.interfaces.Text;

@Named @RequestScoped @Getter
public class CrudOneLevelNamePage extends AbstractCrudOnePage<LevelName> implements Serializable {

	private static final long serialVersionUID = -7933944550331049645L;

	@Override
	protected Class<? extends AbstractFormModel<?>> __formModelClass__() {
		return FormModel.class;
	}
	
	/**/
	
	@Getter @Setter
	public class FormModel extends AbstractFormModel<LevelName> implements Serializable{

		private static final long serialVersionUID = 2063899765621101400L;
		
		@Input @InputText @NotNull
		private String code;
		
		@Input @InputText @NotNull
		private String name;
		
		@Input @InputText @NotNull
		private String abbreviation;
		
		@OutputSeperator(label=@Text(value="markscard")) 
		@Input @InputEditor @NotNull
		private String studentClassroomSessionDivisionResultsReportFile;
		
		@Input @InputTextarea @NotNull
		private String studentClassroomSessionDivisionResultsReportHeadRight;
		
	} 
	
}
