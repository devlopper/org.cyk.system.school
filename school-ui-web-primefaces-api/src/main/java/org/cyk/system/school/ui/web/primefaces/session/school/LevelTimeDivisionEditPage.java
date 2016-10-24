package org.cyk.system.school.ui.web.primefaces.session.school;

import java.io.Serializable;

import javax.faces.view.ViewScoped;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

import org.cyk.system.root.model.time.TimeDivisionType;
import org.cyk.system.school.model.session.Level;
import org.cyk.system.school.model.session.LevelTimeDivision;
import org.cyk.ui.api.model.AbstractBusinessIdentifiedEditFormModel;
import org.cyk.ui.web.primefaces.page.crud.AbstractCrudOnePage;
import org.cyk.utility.common.annotation.user.interfaces.Input;
import org.cyk.utility.common.annotation.user.interfaces.InputChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneChoice;
import org.cyk.utility.common.annotation.user.interfaces.InputOneCombo;

@Named @ViewScoped @Getter @Setter
public class LevelTimeDivisionEditPage extends AbstractCrudOnePage<LevelTimeDivision> implements Serializable {

	private static final long serialVersionUID = 3274187086682750183L;
		
	public static class Form extends AbstractBusinessIdentifiedEditFormModel<LevelTimeDivision> implements Serializable{
		private static final long serialVersionUID = -4741435164709063863L;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo @NotNull private Level level;
		
		@Input @InputChoice @InputOneChoice @InputOneCombo @NotNull private TimeDivisionType timeDivisionType;
		
		/**/
		
		public static final String FIELD_LEVEL = "level";
		public static final String FIELD_TIME_DIVISION_TYPE = "timeDivisionType";
	}

}
